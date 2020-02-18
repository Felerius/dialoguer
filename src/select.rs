use std::cmp;
use std::io;
use std::iter::repeat;
use std::ops::Range;

use theme::{get_default_theme, SelectionStyle, TermThemeRenderer, Theme};

use console::{Key, Term};

struct CommonOptions<'a> {
    prompt: Option<String>,
    clear: bool,
    theme: &'a dyn Theme,
    paged: bool,
    show_cursor: bool,
}

impl<'a> CommonOptions<'a> {
    fn new(theme: &'a dyn Theme) -> Self {
        Self {
            prompt: None,
            clear: true,
            theme,
            paged: false,
            show_cursor: true,
        }
    }
}

/// Renders a selection menu.
pub struct Select<'a> {
    default: usize,
    items: Vec<String>,
    opts: CommonOptions<'a>,
}

/// Renders a multi select checkbox menu.
pub struct Checkboxes<'a> {
    defaults: Vec<bool>,
    items: Vec<String>,
    opts: CommonOptions<'a>,
}

/// Renders a list to order.
pub struct OrderList<'a> {
    items: Vec<String>,
    opts: CommonOptions<'a>,
}

/// Render a list of items and handle movement keys.
struct ListCore<'a> {
    opts: &'a CommonOptions<'a>,
    num_items: usize,
    sel: usize,
    page: usize,
    page_starts: Vec<usize>,
    term_size: (u16, u16),
    rerender_prompt: bool,
    render: TermThemeRenderer<'a>,
}

/// Calculate the width of each line in a string.
fn calc_line_widths<'a>(text: &'a str) -> impl Iterator<Item = usize> + 'a {
    text.split('\n').map(console::measure_text_width)
}

/// Calculate the maximum width for each line of each item.
fn calc_item_line_widths(
    items: &[String],
    theme: &dyn Theme,
    styles: &[SelectionStyle],
) -> Vec<Vec<usize>> {
    // Since themes are free to format items as they see fit (including adding
    // new line breaks, etc.), we defensively format every item for every style,
    // and track the maximum width for every line.
    let mut buf = String::new();
    items
        .iter()
        .map(|item| {
            let mut line_widths = vec![];
            for style in styles {
                buf.clear();
                theme
                    .format_selection(&mut buf, item, *style)
                    .expect("writing to string failed");
                for (idx, width) in calc_line_widths(&buf).enumerate() {
                    if idx == line_widths.len() {
                        line_widths.push(width);
                    } else {
                        line_widths[idx] = cmp::max(line_widths[idx], width);
                    }
                }
            }
            line_widths
        })
        .collect()
}

impl<'a> ListCore<'a> {
    const NO_SELECTION: usize = !0;

    fn new<'b>(
        term: &'a Term,
        opts: &'a CommonOptions<'a>,
        initial_sel: usize,
        item_lines: impl IntoIterator<Item = &'b [usize]>,
    ) -> io::Result<Self> {
        let term_size = term.size();
        term.hide_cursor()?;
        let render = TermThemeRenderer::new(term, opts.theme);
        let mut instance = Self {
            opts,
            num_items: 0,
            sel: initial_sel,
            page: 0,
            page_starts: vec![],
            term_size,
            rerender_prompt: true,
            render,
        };
        instance.recalculate_paging(item_lines);
        Ok(instance)
    }

    fn item_range(&self, page: usize) -> Range<usize> {
        let end = *self.page_starts.get(page + 1).unwrap_or(&self.num_items);
        self.page_starts[page]..end
    }

    fn line_broken_line_count(&self, line_widths: impl IntoIterator<Item = usize>) -> usize {
        let term_width = self.term_size.1 as usize;
        line_widths
            .into_iter()
            .map(|width| (width + term_width - 1) / term_width)
            .sum()
    }

    fn recalculate_paging<'b>(&mut self, item_lines: impl IntoIterator<Item = &'b [usize]>) {
        self.term_size = self.render.term().size();
        let avail_height = if self.opts.paged {
            let height_offset = match self.opts.prompt {
                Some(ref prompt) => self.line_broken_line_count(calc_line_widths(prompt)),
                None => 0,
            };
            self.term_size.0 as usize - height_offset
        } else {
            usize::max_value()
        };

        self.page_starts.clear();
        self.page_starts.push(0);
        self.num_items = 0;
        let mut cur_height = 0;
        for (idx, line_widths) in item_lines.into_iter().enumerate() {
            self.num_items += 1;
            let num_lines = self.line_broken_line_count(line_widths.iter().copied());

            // If a single item is higher than avail_height, give it its own
            // page and hope for the best
            if cur_height + num_lines > avail_height && cur_height > 0 {
                self.page_starts.push(idx);
                cur_height = 0;
            }
            cur_height += num_lines;
        }

        self.page = if self.sel == Self::NO_SELECTION {
            0
        } else {
            self.page_starts
                .iter()
                .skip(1)
                .take_while(|&&idx| idx < self.sel)
                .count()
        };
        self.rerender_prompt = true;
    }

    /// Renders the current state of the list to the terminal.
    ///
    /// Recalculates the paging if the terminal size has changed or if
    /// `force_paging_recalc` is set to `true`.
    fn render<'b>(
        &mut self,
        force_paging_recalc: bool,
        item_lines: impl IntoIterator<Item = &'b [usize]>,
        mut get_item: impl FnMut(usize, bool) -> (&'b str, SelectionStyle),
    ) -> io::Result<()> {
        if force_paging_recalc || self.term_size != self.render.term().size() {
            self.recalculate_paging(item_lines);
        }

        if self.rerender_prompt {
            self.render.clear()?;
            if let Some(prompt) = self.opts.prompt.as_ref() {
                self.render.prompt(prompt)?;
            }
            self.rerender_prompt = false;
        } else {
            self.render.clear_preserve_prompt()?;
        }
        for idx in self.item_range(self.page) {
            let (item, style) = get_item(idx, idx == self.sel);
            if idx + 1 == self.item_range(self.page).end {
                self.render.selection_no_break(item, style)?;
            } else {
                self.render.selection(item, style)?;
            }
        }
        self.render.term().flush()
    }

    /// Clear the screen and show the cursor (both only if requested).
    ///
    /// Do not flush the terminal in case a result line should be printed.
    fn finish(&mut self) -> io::Result<()> {
        if self.opts.clear {
            self.render.clear()?;
        }
        if self.opts.show_cursor {
            self.render.term().show_cursor()?;
        }
        Ok(())
    }

    fn handle_key(&mut self, key: Key) {
        let page_item_range = self.item_range(self.page);
        let num_pages = self.page_starts.len();
        match key {
            Key::ArrowDown | Key::Char('j') => {
                if self.sel == Self::NO_SELECTION {
                    self.sel = 0;
                    self.page = 0;
                } else {
                    if self.sel + 1 == page_item_range.end {
                        self.page = (self.page + 1) % num_pages;
                    }
                    self.sel = (self.sel + 1) % self.num_items;
                }
            }
            Key::ArrowUp | Key::Char('k') => {
                if self.sel == Self::NO_SELECTION {
                    self.sel = self.num_items - 1;
                    self.page = num_pages - 1;
                } else {
                    if self.sel == page_item_range.start {
                        self.page = (self.page + num_pages - 1) % num_pages;
                    }
                    self.sel = (self.sel + self.num_items - 1) % self.num_items;
                }
            }
            Key::ArrowLeft | Key::Char('h') => {
                self.page = (self.page + num_pages - 1) % num_pages;
                self.sel = self.item_range(self.page).start;
            }
            Key::ArrowRight | Key::Char('l') => {
                self.page = (self.page + 1) % num_pages;
                self.sel = self.item_range(self.page).start;
            }
            _ => {}
        }
    }
}

impl<'a> Default for Select<'a> {
    fn default() -> Select<'a> {
        Select::new()
    }
}

impl<'a> Select<'a> {
    /// Creates the prompt with a specific text.
    pub fn new() -> Select<'static> {
        Select::with_theme(get_default_theme())
    }

    /// Same as `new` but with a specific theme.
    pub fn with_theme(theme: &'a dyn Theme) -> Select<'a> {
        Select {
            default: ListCore::NO_SELECTION,
            items: vec![],
            opts: CommonOptions::new(theme),
        }
    }

    /// Enables or disables paging
    pub fn paged(&mut self, val: bool) -> &mut Select<'a> {
        self.opts.paged = val;
        self
    }

    /// Sets the clear behavior of the menu.
    ///
    /// The default is to clear the menu.
    pub fn clear(&mut self, val: bool) -> &mut Select<'a> {
        self.opts.clear = val;
        self
    }

    /// Sets a default for the menu
    pub fn default(&mut self, val: usize) -> &mut Select<'a> {
        self.default = val;
        self
    }

    /// Whether to unhide the cursor when done
    pub fn show_cursor(&mut self, val: bool) -> &mut Select<'a> {
        self.opts.show_cursor = val;
        self
    }

    /// Add a single item to the selector.
    pub fn item(&mut self, item: &str) -> &mut Select<'a> {
        self.items.push(item.to_string());
        self
    }

    /// Adds multiple items to the selector.
    pub fn items<T: ToString>(&mut self, items: &[T]) -> &mut Select<'a> {
        for item in items {
            self.items.push(item.to_string());
        }
        self
    }

    /// Prefaces the menu with a prompt.
    ///
    /// When a prompt is set the system also prints out a confirmation after
    /// the selection.
    pub fn with_prompt(&mut self, prompt: &str) -> &mut Select<'a> {
        self.opts.prompt = Some(prompt.to_string());
        self
    }

    /// Enables user interaction and returns the result.
    ///
    /// The index of the selected item.
    /// The dialog is rendered on stderr.
    pub fn interact(&self) -> io::Result<usize> {
        self.interact_on(&Term::stderr())
    }

    /// Enables user interaction and returns the result.
    ///
    /// The index of the selected item. None if the user
    /// cancelled with Esc or 'q'.
    /// The dialog is rendered on stderr.
    pub fn interact_opt(&self) -> io::Result<Option<usize>> {
        self._interact_on(&Term::stderr(), true)
    }

    /// Like `interact` but allows a specific terminal to be set.
    pub fn interact_on(&self, term: &Term) -> io::Result<usize> {
        self._interact_on(term, false)?
            .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Quit not allowed in this case"))
    }

    /// Like `interact_opt` but allows a specific terminal to be set.
    pub fn interact_on_opt(&self, term: &Term) -> io::Result<Option<usize>> {
        self._interact_on(term, true)
    }

    /// Like `interact` but allows a specific terminal to be set.
    fn _interact_on(&self, term: &Term, allow_quit: bool) -> io::Result<Option<usize>> {
        let item_lines = calc_item_line_widths(
            &self.items,
            self.opts.theme,
            &[SelectionStyle::MenuSelected, SelectionStyle::MenuUnselected],
        );
        let mut list = ListCore::new(
            term,
            &self.opts,
            self.default,
            item_lines.iter().map(|v| &v[..]),
        )?;
        let idx = loop {
            list.render(false, item_lines.iter().map(|v| &v[..]), |idx, selected| {
                (
                    &self.items[idx],
                    if selected {
                        SelectionStyle::MenuSelected
                    } else {
                        SelectionStyle::MenuUnselected
                    },
                )
            })?;
            match term.read_key()? {
                Key::Escape | Key::Char('q') => {
                    if allow_quit {
                        list.finish()?;
                        break None;
                    }
                }
                Key::Enter | Key::Char(' ') if list.sel != ListCore::NO_SELECTION => {
                    list.finish()?;
                    if let Some(ref prompt) = self.opts.prompt {
                        list.render
                            .single_prompt_selection(prompt, &self.items[list.sel])?;
                    }
                    break Some(list.sel);
                }
                key => list.handle_key(key),
            }
        };
        term.flush()?;
        Ok(idx)
    }
}

impl<'a> Default for Checkboxes<'a> {
    fn default() -> Checkboxes<'a> {
        Checkboxes::new()
    }
}

impl<'a> Checkboxes<'a> {
    /// Creates a new checkbox object.
    pub fn new() -> Checkboxes<'static> {
        Checkboxes::with_theme(get_default_theme())
    }

    /// Sets a theme other than the default one.
    pub fn with_theme(theme: &'a dyn Theme) -> Checkboxes<'a> {
        Checkboxes {
            items: vec![],
            defaults: vec![],
            opts: CommonOptions::new(theme),
        }
    }

    /// Enables or disables paging
    pub fn paged(&mut self, val: bool) -> &mut Checkboxes<'a> {
        self.opts.paged = val;
        self
    }

    /// Sets the clear behavior of the checkbox menu.
    ///
    /// The default is to clear the checkbox menu.
    pub fn clear(&mut self, val: bool) -> &mut Checkboxes<'a> {
        self.opts.clear = val;
        self
    }

    /// Whether to unhide the cursor when done
    pub fn show_cursor(&mut self, val: bool) -> &mut Checkboxes<'a> {
        self.opts.show_cursor = val;
        self
    }

    /// Sets a defaults for the menu
    pub fn defaults(&mut self, val: &[bool]) -> &mut Checkboxes<'a> {
        self.defaults = val
            .iter()
            .cloned()
            .chain(repeat(false))
            .take(self.items.len())
            .collect();
        self
    }

    /// Add a single item to the selector.
    pub fn item(&mut self, item: &str) -> &mut Checkboxes<'a> {
        self.item_checked(item, false)
    }

    /// Add a single item to the selector with a default checked state.
    pub fn item_checked(&mut self, item: &str, checked: bool) -> &mut Checkboxes<'a> {
        self.items.push(item.to_string());
        self.defaults.push(checked);
        self
    }

    /// Adds multiple items to the selector.
    pub fn items<T: ToString>(&mut self, items: &[T]) -> &mut Checkboxes<'a> {
        for item in items {
            self.items.push(item.to_string());
            self.defaults.push(false);
        }
        self
    }

    /// Adds multiple items to the selector with checked state
    pub fn items_checked<T: ToString>(&mut self, items: &[(T, bool)]) -> &mut Checkboxes<'a> {
        for &(ref item, checked) in items {
            self.items.push(item.to_string());
            self.defaults.push(checked);
        }
        self
    }

    /// Prefaces the menu with a prompt.
    ///
    /// When a prompt is set the system also prints out a confirmation after
    /// the selection.
    pub fn with_prompt(&mut self, prompt: &str) -> &mut Checkboxes<'a> {
        self.opts.prompt = Some(prompt.to_string());
        self
    }

    /// Enables user interaction and returns the result.
    ///
    /// The user can select the items with the space bar and on enter
    /// the selected items will be returned.
    pub fn interact(&self) -> io::Result<Vec<usize>> {
        self.interact_on(&Term::stderr())
    }

    /// Like `interact` but allows a specific terminal to be set.
    pub fn interact_on(&self, term: &Term) -> io::Result<Vec<usize>> {
        let item_lines = calc_item_line_widths(
            &self.items,
            self.opts.theme,
            &[
                SelectionStyle::CheckboxCheckedSelected,
                SelectionStyle::CheckboxCheckedUnselected,
                SelectionStyle::CheckboxUncheckedSelected,
                SelectionStyle::CheckboxUncheckedUnselected,
            ],
        );
        let mut list = ListCore::new(term, &self.opts, 0, item_lines.iter().map(|v| &v[..]))?;
        let mut checked = self.defaults.clone();
        let indices = loop {
            list.render(false, item_lines.iter().map(|v| &v[..]), |idx, selected| {
                (
                    &self.items[idx],
                    match (checked[idx], selected) {
                        (true, true) => SelectionStyle::CheckboxCheckedSelected,
                        (true, false) => SelectionStyle::CheckboxCheckedUnselected,
                        (false, true) => SelectionStyle::CheckboxUncheckedSelected,
                        (false, false) => SelectionStyle::CheckboxUncheckedUnselected,
                    },
                )
            })?;
            match term.read_key()? {
                Key::Char(' ') => {
                    checked[list.sel] = !checked[list.sel];
                }
                Key::Escape => {
                    list.finish()?;
                    if let Some(ref prompt) = self.opts.prompt {
                        list.render.multi_prompt_selection(prompt, &[][..])?;
                    }
                    break (0..self.items.len())
                        .filter(|&i| self.defaults[i])
                        .collect();
                }
                Key::Enter => {
                    list.finish()?;
                    let checked_indices = (0..self.items.len()).filter(|&i| checked[i]);
                    if let Some(ref prompt) = self.opts.prompt {
                        let selections: Vec<_> = checked_indices
                            .clone()
                            .map(|i| self.items[i].as_str())
                            .collect();
                        list.render
                            .multi_prompt_selection(prompt, &selections[..])?;
                    }
                    break checked_indices.collect();
                }
                key => list.handle_key(key),
            }
        };
        term.flush()?;
        Ok(indices)
    }
}

impl<'a> Default for OrderList<'a> {
    fn default() -> OrderList<'a> {
        OrderList::new()
    }
}

impl<'a> OrderList<'a> {
    /// Creates a new orderlist object.
    pub fn new() -> OrderList<'static> {
        OrderList::with_theme(get_default_theme())
    }

    /// Sets a theme other than the default one.
    pub fn with_theme(theme: &'a dyn Theme) -> OrderList<'a> {
        OrderList {
            items: vec![],
            opts: CommonOptions::new(theme),
        }
    }

    /// Enables or disables paging
    pub fn paged(&mut self, val: bool) -> &mut OrderList<'a> {
        self.opts.paged = val;
        self
    }

    /// Sets the clear behavior of the checkbox menu.
    ///
    /// The default is to clear the checkbox menu.
    pub fn clear(&mut self, val: bool) -> &mut OrderList<'a> {
        self.opts.clear = val;
        self
    }

    /// Whether to unhide the cursor when done
    pub fn show_cursor(&mut self, val: bool) -> &mut OrderList<'a> {
        self.opts.show_cursor = val;
        self
    }

    /// Add a single item to the selector.
    pub fn item(&mut self, item: &str) -> &mut OrderList<'a> {
        self.items.push(item.to_string());
        self
    }

    /// Adds multiple items to the selector.
    pub fn items<T: ToString>(&mut self, items: &[T]) -> &mut OrderList<'a> {
        for item in items {
            self.items.push(item.to_string());
        }
        self
    }

    /// Prefaces the menu with a prompt.
    ///
    /// When a prompt is set the system also prints out a confirmation after
    /// the selection.
    pub fn with_prompt(&mut self, prompt: &str) -> &mut OrderList<'a> {
        self.opts.prompt = Some(prompt.to_string());
        self
    }

    /// Enables user interaction and returns the result.
    ///
    /// The user can order the items with the space bar and the arrows.
    /// On enter the ordered list will be returned.
    pub fn interact(&self) -> io::Result<Vec<usize>> {
        self.interact_on(&Term::stderr())
    }

    /// Like `interact` but allows a specific terminal to be set.
    pub fn interact_on(&self, term: &Term) -> io::Result<Vec<usize>> {
        let item_lines = calc_item_line_widths(
            &self.items,
            self.opts.theme,
            &[SelectionStyle::MenuSelected, SelectionStyle::MenuUnselected],
        );
        let mut list = ListCore::new(term, &self.opts, 0, item_lines.iter().map(|v| &v[..]))?;

        let mut order: Vec<_> = (0..self.items.len()).collect();
        let mut checked: bool = false;
        let mut force_paging_recalc = false;
        loop {
            list.render(
                force_paging_recalc,
                order.iter().map(|&i| &item_lines[i][..]),
                |idx, selected| {
                    (
                        &self.items[order[idx]],
                        match (selected, checked) {
                            (true, true) => SelectionStyle::CheckboxCheckedSelected,
                            (true, false) => SelectionStyle::CheckboxUncheckedSelected,
                            (false, _) => SelectionStyle::CheckboxUncheckedUnselected,
                        },
                    )
                },
            )?;
            force_paging_recalc = false;

            let key = term.read_key()?;
            match key {
                Key::ArrowDown | Key::ArrowUp | Key::Char('j') | Key::Char('k') => {
                    let old_sel = list.sel;
                    let old_page = list.page;
                    list.handle_key(key);
                    if checked && old_sel != list.sel {
                        order.swap(old_sel, list.sel);
                        // If the item stays on the same page, the number of
                        // lines per page haven't changed and there is no need
                        // to recalculate
                        if old_page != list.page {
                            force_paging_recalc = true;
                        }
                    }
                }
                Key::ArrowLeft | Key::ArrowRight | Key::Char('h') | Key::Char('l') => {
                    let old_sel = list.sel;
                    list.handle_key(key);
                    if checked {
                        if list.sel < old_sel {
                            order[list.sel..=old_sel].rotate_right(1);
                        } else if list.sel > old_sel {
                            order[old_sel..=list.sel].rotate_left(1);
                        }
                        if list.sel != old_sel {
                            force_paging_recalc = true;
                        }
                    }
                }
                Key::Char(' ') => {
                    checked = !checked;
                }
                Key::Enter => {
                    list.finish()?;
                    if let Some(ref prompt) = self.opts.prompt {
                        let item_list: Vec<_> =
                            order.iter().map(|&idx| self.items[idx].as_str()).collect();
                        list.render.multi_prompt_selection(prompt, &item_list[..])?;
                    }
                    break;
                }
                _ => {}
            }
        }
        term.flush()?;
        Ok(order)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_str() {
        let selections = &[
            "Ice Cream",
            "Vanilla Cupcake",
            "Chocolate Muffin",
            "A Pile of sweet, sweet mustard",
        ];

        assert_eq!(
            Select::new().default(0).items(&selections[..]).items,
            selections
        );
    }

    #[test]
    fn test_string() {
        let selections = vec!["a".to_string(), "b".to_string()];

        assert_eq!(
            Select::new().default(0).items(&selections[..]).items,
            selections
        );
    }

    #[test]
    fn test_ref_str() {
        let a = "a";
        let b = "b";

        let selections = &[a, b];

        assert_eq!(
            Select::new().default(0).items(&selections[..]).items,
            selections
        );
    }
}
