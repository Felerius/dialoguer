//! Customizes the rendering of the elements.
use std::cmp;
use std::fmt;
use std::io;

use console::{Style, Term};

/// Rendering style for a selected item
#[derive(Debug, Clone, Copy)]
pub enum SelectionStyle {
    /// Renders an unchecked but selected checkbox
    CheckboxUncheckedSelected,
    /// Renders an unchecked and unselected checkbox
    CheckboxUncheckedUnselected,
    /// Renders a checked but selected checkbox
    CheckboxCheckedSelected,
    /// Renders a checked and unselected checkbox
    CheckboxCheckedUnselected,
    /// Renders a selected menu item
    MenuSelected,
    /// Renders un unselected menu item
    MenuUnselected,
}

/// Implements a theme for dialoguer.
pub trait Theme {
    /// Given a prompt this formats out what the prompt should look like (multiline).
    fn format_prompt(&self, f: &mut dyn fmt::Write, prompt: &str) -> fmt::Result {
        write!(f, "{}:", prompt)
    }

    /// Given a prompt this formats out what the prompt should look like (singleline).
    fn format_singleline_prompt(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
        default: Option<&str>,
    ) -> fmt::Result {
        match default {
            Some(default) => write!(f, "{} [{}]: ", prompt, default),
            None => write!(f, "{}: ", prompt),
        }
    }

    /// Formats out an error.
    fn format_error(&self, f: &mut dyn fmt::Write, err: &str) -> fmt::Result {
        write!(f, "error: {}", err)
    }

    /// Formats a confirmation prompt.
    fn format_confirmation_prompt(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
        default: Option<bool>,
    ) -> fmt::Result {
        write!(f, "{}", &prompt)?;
        match default {
            None => {}
            Some(true) => write!(f, " [Y/n] ")?,
            Some(false) => write!(f, " [y/N] ")?,
        }
        Ok(())
    }

    /// Formats a confirmation prompt.
    fn format_confirmation_prompt_selection(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
        selection: bool,
    ) -> fmt::Result {
        write!(f, "{} {}", &prompt, if selection { "yes" } else { "no" })
    }

    /// Renders a prompt and a single selection made.
    fn format_single_prompt_selection(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
        sel: &str,
    ) -> fmt::Result {
        write!(f, "{}: {}", prompt, sel)
    }

    /// Renders a prompt and multiple selections,
    fn format_multi_prompt_selection(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
        selections: &[&str],
    ) -> fmt::Result {
        write!(f, "{}: ", prompt)?;
        for (idx, sel) in selections.iter().enumerate() {
            write!(f, "{}{}", if idx == 0 { "" } else { ", " }, sel)?;
        }
        Ok(())
    }

    /// Renders a prompt and multiple selections,
    fn format_password_prompt_selection(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
    ) -> fmt::Result {
        self.format_single_prompt_selection(f, prompt, "[hidden]")
    }

    /// Formats a selection.
    fn format_selection(
        &self,
        f: &mut dyn fmt::Write,
        text: &str,
        style: SelectionStyle,
    ) -> fmt::Result {
        write!(
            f,
            "{}{}",
            match style {
                SelectionStyle::CheckboxUncheckedSelected => "> [ ] ",
                SelectionStyle::CheckboxUncheckedUnselected => "  [ ] ",
                SelectionStyle::CheckboxCheckedSelected => "> [x] ",
                SelectionStyle::CheckboxCheckedUnselected => "  [x] ",
                SelectionStyle::MenuSelected => "> ",
                SelectionStyle::MenuUnselected => "  ",
            },
            text
        )
    }
}

/// The default theme.
pub struct SimpleTheme;

impl Theme for SimpleTheme {}
/// The default theme, with a custom prompt character in place of `:`
pub struct CustomPromptCharacterTheme {
    prompt_character: char,
}
impl CustomPromptCharacterTheme {
    /// Creates a theme, the prompt character for which is customized
    pub fn new(character: char) -> CustomPromptCharacterTheme {
        CustomPromptCharacterTheme {
            prompt_character: character,
        }
    }
}
impl Default for CustomPromptCharacterTheme {
    fn default() -> Self {
        CustomPromptCharacterTheme {
            prompt_character: ':',
        }
    }
}
impl Theme for CustomPromptCharacterTheme {
    /// Given a prompt this formats out what the prompt should look like (multiline).
    fn format_prompt(&self, f: &mut dyn fmt::Write, prompt: &str) -> fmt::Result {
        write!(f, "{}{}", prompt, self.prompt_character)
    }

    /// Given a prompt this formats out what the prompt should look like (singleline).
    fn format_singleline_prompt(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
        default: Option<&str>,
    ) -> fmt::Result {
        match default {
            Some(default) => write!(f, "{} [{}]{} ", prompt, default, self.prompt_character),
            None => write!(f, "{}{} ", prompt, self.prompt_character),
        }
    }
    /// Renders a prompt and a single selection made.
    fn format_single_prompt_selection(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
        sel: &str,
    ) -> fmt::Result {
        write!(f, "{}{} {}", prompt, self.prompt_character, sel)
    }

    /// Renders a prompt and multiple selections,
    fn format_multi_prompt_selection(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
        selections: &[&str],
    ) -> fmt::Result {
        write!(f, "{}{} ", prompt, self.prompt_character)?;
        for (idx, sel) in selections.iter().enumerate() {
            write!(f, "{}{}", if idx == 0 { "" } else { ", " }, sel)?;
        }
        Ok(())
    }
}
/// A colorful theme
pub struct ColorfulTheme {
    /// The style for default values in prompts and similar
    pub defaults_style: Style,
    /// The style for errors indicators
    pub error_style: Style,
    /// The style for user interface indicators
    pub indicator_style: Style,
    /// The style for inactive elements
    pub inactive_style: Style,
    /// The style for active elements
    pub active_style: Style,
    /// The style for values indicating "yes"
    pub yes_style: Style,
    /// The style for values indicating "no"
    pub no_style: Style,
    /// The style for values embedded in prompts
    pub values_style: Style,
}

impl Default for ColorfulTheme {
    fn default() -> ColorfulTheme {
        ColorfulTheme {
            defaults_style: Style::new().dim(),
            error_style: Style::new().red(),
            indicator_style: Style::new().cyan().bold(),
            inactive_style: Style::new().dim(),
            active_style: Style::new(),
            yes_style: Style::new().green(),
            no_style: Style::new().green(),
            values_style: Style::new().cyan(),
        }
    }
}

impl Theme for ColorfulTheme {
    fn format_prompt(&self, f: &mut dyn fmt::Write, prompt: &str) -> fmt::Result {
        write!(f, "{}:", prompt)
    }

    fn format_singleline_prompt(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
        default: Option<&str>,
    ) -> fmt::Result {
        match default {
            Some(default) => write!(
                f,
                "{} [{}]: ",
                prompt,
                self.defaults_style.apply_to(default)
            ),
            None => write!(f, "{}: ", prompt),
        }
    }

    fn format_error(&self, f: &mut dyn fmt::Write, err: &str) -> fmt::Result {
        write!(f, "{}: {}", self.error_style.apply_to("error"), err)
    }

    fn format_confirmation_prompt(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
        default: Option<bool>,
    ) -> fmt::Result {
        write!(f, "{}", &prompt)?;
        match default {
            None => {}
            Some(true) => write!(f, " {} ", self.defaults_style.apply_to("[Y/n]"))?,
            Some(false) => write!(f, " {} ", self.defaults_style.apply_to("[y/N]"))?,
        }
        Ok(())
    }

    fn format_confirmation_prompt_selection(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
        selection: bool,
    ) -> fmt::Result {
        write!(
            f,
            "{} {}",
            &prompt,
            if selection {
                self.yes_style.apply_to("yes")
            } else {
                self.no_style.apply_to("no")
            }
        )
    }

    fn format_single_prompt_selection(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
        sel: &str,
    ) -> fmt::Result {
        write!(f, "{}: {}", prompt, self.values_style.apply_to(sel))
    }

    fn format_multi_prompt_selection(
        &self,
        f: &mut dyn fmt::Write,
        prompt: &str,
        selections: &[&str],
    ) -> fmt::Result {
        write!(f, "{}: ", prompt)?;
        for (idx, sel) in selections.iter().enumerate() {
            write!(
                f,
                "{}{}",
                if idx == 0 { "" } else { ", " },
                self.values_style.apply_to(sel)
            )?;
        }
        Ok(())
    }

    fn format_selection(
        &self,
        f: &mut dyn fmt::Write,
        text: &str,
        st: SelectionStyle,
    ) -> fmt::Result {
        match st {
            SelectionStyle::CheckboxUncheckedSelected => write!(
                f,
                "{} [ ] {}",
                self.indicator_style.apply_to(">"),
                self.active_style.apply_to(text)
            ),
            SelectionStyle::CheckboxUncheckedUnselected => {
                write!(f, "  [ ] {}", self.inactive_style.apply_to(text))
            }
            SelectionStyle::CheckboxCheckedSelected => write!(
                f,
                "{} [{}] {}",
                self.indicator_style.apply_to(">"),
                self.indicator_style.apply_to("x"),
                self.active_style.apply_to(text),
            ),
            SelectionStyle::CheckboxCheckedUnselected => write!(
                f,
                "  [{}] {}",
                self.indicator_style.apply_to("x"),
                self.inactive_style.apply_to(text)
            ),
            SelectionStyle::MenuSelected => write!(
                f,
                "{} {}",
                self.indicator_style.apply_to(">"),
                self.active_style.apply_to(text)
            ),
            SelectionStyle::MenuUnselected => write!(f, "  {}", self.inactive_style.apply_to(text)),
        }
    }
}

/// Helper struct to conveniently render a theme ot a term.
pub(crate) struct TermThemeRenderer<'a> {
    term: &'a Term,
    theme: &'a dyn Theme,
    line_widths: Vec<usize>,
    cur_line_width: usize,
    prompt_height: usize,
    prompts_reset_height: bool,
}

impl<'a> TermThemeRenderer<'a> {
    pub fn new(term: &'a Term, theme: &'a dyn Theme) -> TermThemeRenderer<'a> {
        TermThemeRenderer {
            term,
            theme,
            line_widths: vec![],
            cur_line_width: 0,
            prompt_height: 0,
            prompts_reset_height: true,
        }
    }

    fn wrapped_line_height(line_width: usize, term_width: usize) -> usize {
        let height = (line_width + term_width - 1) / term_width;
        // Make sure that an empty line still counts as height 1
        cmp::max(1, height)
    }

    pub fn set_prompts_reset_height(&mut self, val: bool) {
        self.prompts_reset_height = val;
    }

    pub fn term(&self) -> &Term {
        self.term
    }

    pub fn add_width(&mut self, text: &str) {
        self.cur_line_width += console::measure_text_width(text);
    }

    pub fn break_line(&mut self) {
        self.line_widths.push(self.cur_line_width);
        self.cur_line_width = 0;
    }

    fn add_lines_from(&mut self, buf: &str) {
        let mut line_iter = buf.split('\n');
        if let Some(last_line) = line_iter.next_back() {
            for line in line_iter {
                self.add_width(line);
                self.break_line();
            }
            self.add_width(last_line);
        }
    }

    fn write_formatted_str<
        F: FnOnce(&mut TermThemeRenderer, &mut dyn fmt::Write) -> fmt::Result,
    >(
        &mut self,
        f: F,
    ) -> io::Result<()> {
        let mut buf = String::new();
        f(self, &mut buf).map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
        self.add_lines_from(&buf);
        self.term.write_str(&buf)
    }

    fn write_formatted_line<
        F: FnOnce(&mut TermThemeRenderer, &mut dyn fmt::Write) -> fmt::Result,
    >(
        &mut self,
        f: F,
    ) -> io::Result<()> {
        let mut buf = String::new();
        f(self, &mut buf).map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
        self.add_lines_from(&buf);
        self.break_line();
        self.term.write_line(&buf)
    }

    fn write_formatted_prompt<
        F: FnOnce(&mut TermThemeRenderer, &mut dyn fmt::Write) -> fmt::Result,
    >(
        &mut self,
        f: F,
    ) -> io::Result<()> {
        self.write_formatted_line(f)?;
        if self.prompts_reset_height {
            self.prompt_height = self.line_widths.len();
        }
        Ok(())
    }

    pub fn error(&mut self, err: &str) -> io::Result<()> {
        self.write_formatted_line(|this, buf| this.theme.format_error(buf, err))
    }

    pub fn prompt(&mut self, prompt: &str) -> io::Result<()> {
        self.write_formatted_prompt(|this, buf| this.theme.format_prompt(buf, prompt))
    }

    pub fn input_prompt(&mut self, prompt: &str, default: Option<&str>) -> io::Result<()> {
        self.write_formatted_str(|this, buf| {
            this.theme.format_singleline_prompt(buf, prompt, default)
        })
    }

    pub fn password_prompt(&mut self, prompt: &str) -> io::Result<()> {
        self.write_formatted_str(|this, buf| {
            write!(buf, "\r")?;
            this.theme.format_singleline_prompt(buf, prompt, None)
        })
    }

    pub fn confirmation_prompt(&mut self, prompt: &str, default: Option<bool>) -> io::Result<()> {
        self.write_formatted_str(|this, buf| {
            this.theme.format_confirmation_prompt(buf, prompt, default)
        })
    }

    pub fn confirmation_prompt_selection(&mut self, prompt: &str, sel: bool) -> io::Result<()> {
        self.write_formatted_prompt(|this, buf| {
            this.theme
                .format_confirmation_prompt_selection(buf, prompt, sel)
        })
    }

    pub fn single_prompt_selection(&mut self, prompt: &str, sel: &str) -> io::Result<()> {
        self.write_formatted_prompt(|this, buf| {
            this.theme.format_single_prompt_selection(buf, prompt, sel)
        })
    }

    pub fn multi_prompt_selection(&mut self, prompt: &str, selections: &[&str]) -> io::Result<()> {
        self.write_formatted_prompt(|this, buf| {
            this.theme
                .format_multi_prompt_selection(buf, prompt, selections)
        })
    }

    pub fn password_prompt_selection(&mut self, prompt: &str) -> io::Result<()> {
        self.write_formatted_prompt(|this, buf| {
            this.theme.format_password_prompt_selection(buf, prompt)
        })
    }

    pub fn selection(&mut self, text: &str, style: SelectionStyle) -> io::Result<()> {
        self.write_formatted_line(|this, buf| this.theme.format_selection(buf, text, style))
    }

    fn clear_last_lines(&mut self, n: usize) -> io::Result<()> {
        let term_width = self.term.size().1 as usize;
        let num_lines = self
            .line_widths
            .iter()
            .rev()
            .take(n)
            .map(|&line_width| Self::wrapped_line_height(line_width, term_width))
            .sum();
        self.term.clear_last_lines(num_lines)?;
        self.line_widths.truncate(self.line_widths.len() - n);
        Ok(())
    }

    pub fn clear_line(&mut self) -> io::Result<()> {
        if self.cur_line_width == 0 {
            return Ok(());
        }
        let term_width = self.term.size().1 as usize;
        let line_height = Self::wrapped_line_height(self.cur_line_width, term_width);
        self.term.clear_line()?;
        if line_height > 1 {
            self.term.clear_last_lines(line_height - 1)?;
        }
        self.cur_line_width = 0;
        Ok(())
    }

    pub fn clear(&mut self) -> io::Result<()> {
        self.prompt_height = 0;
        self.clear_line()?;
        self.clear_last_lines(self.line_widths.len())
    }

    pub fn clear_preserve_prompt(&mut self) -> io::Result<()> {
        self.clear_line()?;
        self.clear_last_lines(self.line_widths.len() - self.prompt_height)
    }
}

/// Returns the default theme.
///
/// (This returns the simple theme)
pub(crate) fn get_default_theme() -> &'static dyn Theme {
    &SimpleTheme
}
