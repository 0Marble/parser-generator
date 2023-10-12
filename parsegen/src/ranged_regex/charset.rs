use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Char {
    Min,
    Max,
    Char(char),
}

impl Char {
    pub fn from_u32(v: u32) -> Option<Self> {
        match v {
            u32::MIN => Some(Self::Min),
            u32::MAX => Some(Self::Max),
            v => Some(Self::Char(char::from_u32(v)?)),
        }
    }
    pub fn next(&self) -> Self {
        if self.is_max() {
            return self.clone();
        }
        Self::next_u32(self.as_u32() + 1)
    }
    pub fn next_u32(v: u32) -> Self {
        (v..=u32::MAX)
            .find_map(|a| Char::from_u32(a))
            .expect("u32::MAX is always valid")
    }
    pub fn prev(&self) -> Self {
        if self.is_min() {
            return self.clone();
        }
        Self::prev_u32(self.as_u32() - 1)
    }
    pub fn prev_u32(v: u32) -> Self {
        (u32::MIN..=v)
            .rev()
            .find_map(|a| Char::from_u32(a))
            .expect("u32::MIN is always valid")
    }
    pub fn as_u32(self) -> u32 {
        match self {
            Char::Min => u32::MIN,
            Char::Max => u32::MAX,
            Char::Char(c) => c as _,
        }
    }

    /// Returns `true` if the char is [`Min`].
    ///
    /// [`Min`]: Char::Min
    #[must_use]
    pub fn is_min(&self) -> bool {
        matches!(self, Self::Min)
    }

    /// Returns `true` if the char is [`Max`].
    ///
    /// [`Max`]: Char::Max
    #[must_use]
    pub fn is_max(&self) -> bool {
        matches!(self, Self::Max)
    }

    /// Returns `true` if the char is [`Char`].
    ///
    /// [`Char`]: Char::Char
    #[must_use]
    pub fn is_char(&self) -> bool {
        matches!(self, Self::Char(..))
    }
}

impl From<char> for Char {
    fn from(value: char) -> Self {
        Self::from_u32(value as u32).unwrap()
    }
}

#[derive(Debug, Clone, Default)]
pub struct CharSet {
    front_ranges: Vec<(u32, u32)>,
    back_ranges: Vec<(u32, u32)>,
}

impl PartialEq for CharSet {
    fn eq(&self, other: &Self) -> bool {
        self.front_ranges
            .iter()
            .zip(other.front_ranges.iter())
            .all(|((a, b), (c, d))| a == c && b == d)
    }
}

impl Display for CharSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (a, b) in self.ranges() {
            match (a, b) {
                (Char::Min, Char::Min) => write!(f, "[<]")?,
                (Char::Min, Char::Char(c)) => write!(f, "[..{}]", c)?,
                (Char::Min, Char::Max) => write!(f, "[..]")?,
                (Char::Char(c), Char::Char(d)) => write!(f, "[{}..{}]", c, d)?,
                (Char::Char(c), Char::Max) => write!(f, "[{}..]", c)?,
                (Char::Max, Char::Max) => write!(f, "[>]")?,
                _ => unreachable!("invalid range: ({:?},{:?})", a, b),
            }
            //
        }
        Ok(())
    }
}

impl FromIterator<char> for CharSet {
    fn from_iter<T: IntoIterator<Item = char>>(iter: T) -> Self {
        let mut n = Self::new();
        for c in iter {
            n.add_single(c);
        }
        n
    }
}

impl CharSet {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn alpha() -> Self {
        let mut n = Self::new();
        n.add_minmax('a', 'z');
        n.add_minmax('A', 'Z');
        n
    }
    pub fn alnum() -> Self {
        let mut n = Self::alpha();
        n.add_minmax('0', '9');
        n
    }
    pub fn numbers() -> Self {
        let mut n = Self::new();
        n.add_minmax('0', '9');
        n
    }

    fn add(&mut self, mut a: u32, mut b: u32) {
        assert!(Char::from_u32(a).is_some() && Char::from_u32(b).is_some());
        assert!(a <= b);

        let mut pushed = false;
        std::mem::swap(&mut self.front_ranges, &mut self.back_ranges);
        self.front_ranges.clear();

        for (c, d) in &self.back_ranges {
            let c = *c;
            let d = *d;

            let after_d = if d < u32::MAX {
                Char::next_u32(d + 1).as_u32()
            } else {
                d
            };
            if after_d < a || pushed {
                self.front_ranges.push((c, d));
                continue;
            }

            let after_b = if b < u32::MAX {
                Char::next_u32(b + 1).as_u32()
            } else {
                b
            };
            if after_b < c && !pushed {
                self.front_ranges.push((a, b));
                self.front_ranges.push((c, d));
                pushed = true;
                continue;
            }

            assert!(a >= c && a <= d || b >= c && b <= d || after_b == c || after_d == a);
            a = u32::min(a, c);
            b = u32::max(b, d);
        }

        if !pushed {
            self.front_ranges.push((a, b));
        }
    }

    pub fn add_min(&mut self, a: char) -> &mut Self {
        self.add(a as _, u32::MAX);
        self
    }
    pub fn add_max(&mut self, b: char) -> &mut Self {
        self.add(u32::MIN, b as _);
        self
    }
    pub fn add_minmax(&mut self, min: char, max: char) -> &mut Self {
        let a = u32::min(min as _, max as _);
        let b = u32::max(min as _, max as _);
        self.add(a, b);
        self
    }
    pub fn add_single(&mut self, a: char) -> &mut Self {
        self.add(a as _, a as _);
        self
    }

    pub fn ranges(&self) -> impl Iterator<Item = (Char, Char)> + '_ {
        self.front_ranges
            .iter()
            .map(|(a, b)| (Char::from_u32(*a).unwrap(), Char::from_u32(*b).unwrap()))
    }

    pub fn contains(&self, c: char) -> bool {
        let c = c as u32;
        for (a, b) in &self.front_ranges {
            if *a <= c && *b >= c {
                return true;
            }
        }

        false
    }
    pub fn is_empty(&self) -> bool {
        self.front_ranges.is_empty()
    }
    pub fn union(&self, other: &Self) -> Self {
        let mut n = self.clone();
        for (a, b) in &other.front_ranges {
            n.add(*a, *b);
        }
        n
    }
    pub fn intersection(&self, other: &Self) -> Self {
        let mut i = 0;
        let mut j = 0;
        let mut n = Self::new();

        loop {
            if i >= self.front_ranges.len() || j >= other.front_ranges.len() {
                break;
            }

            let (a, b) = self.front_ranges[i];
            let (c, d) = other.front_ranges[j];

            if b < c {
                i += 1;
                continue;
            }
            if a > d {
                j += 1;
                continue;
            }

            assert!(a >= c && a <= d || b >= c && b <= d);
            let from = u32::max(a, c);
            let to = u32::min(b, d);
            n.add(from, to);
            if b < d {
                i += 1;
            } else {
                j += 1;
            }
        }

        n
    }
    pub fn setminus(&self, other: &Self) -> Self {
        self.intersection(&other.complement())
    }
    pub fn complement(&self) -> Self {
        let mut n = Self::new();
        let mut a = u32::MIN;
        let mut b = u32::MAX;
        if self.is_empty() {
            n.add(a, b);
            return n;
        }

        for (c, d) in &self.front_ranges {
            let c = *c;
            let d = *d;

            if a < c {
                b = Char::prev_u32(c - 1).as_u32();
                n.add(a, b);
            }
            a = if d < u32::MAX {
                Char::next_u32(d + 1).as_u32()
            } else {
                return n;
            }
        }

        n.add(a, u32::MAX);

        n
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_ranges() {
        let alnum_range = CharSet::alnum();
        assert_eq!(alnum_range.to_string(), "[0..9][A..Z][a..z]");
        for c in ('a'..='z').chain('A'..='Z').chain('0'..='9') {
            assert!(alnum_range.contains(c));
        }
        for c in ['-', '<', 'ф', '哈', '💝'] {
            assert!(!alnum_range.contains(c));
        }

        let mut a = CharSet::from_iter(['a', 'c', 'e']);
        assert_eq!(a.to_string(), "[a..a][c..c][e..e]");
        a.add_single('b').add_single('d');
        assert_eq!(a.to_string(), "[a..e]");
    }

    #[test]
    fn unicode_chars() {
        let mut a = CharSet::new();
        a.add_minmax('а', 'я');
        a.add_minmax('一', '俿');
        a = a.setminus(&CharSet::new().add_single('们'));
        assert_eq!(a.to_string(), "[а..я][一..仫][仭..俿]");
    }

    #[test]
    fn range_ops() {
        let a_chars = ['a', 'b', 'c', '1', '2', '3'];
        let b_chars = ['a', 'x', 'y', '1', '9', '0'];
        let a = CharSet::from_iter(a_chars);
        let b = CharSet::from_iter(b_chars);

        assert_eq!(a.to_string(), "[1..3][a..c]");
        assert_eq!(b.to_string(), "[0..1][9..9][a..a][x..y]");

        let a_or_b = a.union(&b);
        for c in a_chars.iter().chain(b_chars.iter()).cloned() {
            assert!(a_or_b.contains(c), "set={}, c={}", a_or_b, c);
        }
        for c in ['e', 'q', 'z', '8'] {
            assert!(!a_or_b.contains(c), "set={}, c={}", a_or_b, c);
        }

        let a_and_b = a.intersection(&b);
        for c in ['a', '1'] {
            assert!(a_and_b.contains(c), "set={}, c={}", a_and_b, c);
        }
        for c in ['b', 'c', '2', '3', 'x', 'y', '9', '0', 'g', '-', 'z'] {
            assert!(!a_and_b.contains(c), "set={}, c={}", a_and_b, c);
        }

        let not_a = a.complement();
        assert_eq!(not_a.to_string(), "[..0][4..`][d..]");
        assert_eq!(not_a.complement(), a, "{}, {}", not_a.complement(), a);

        for c in a_chars {
            assert!(!not_a.contains(c), "set={}, c={}", not_a, c);
        }
        for c in ['q', 'e', '$', 'p'] {
            assert!(not_a.contains(c), "set={}, c={}", not_a, c);
        }

        let a_minus_b = a.setminus(&b);
        for c in ['b', 'c', '2', '3'] {
            assert!(a_minus_b.contains(c), "set={}, c={}", a_minus_b, c);
        }
        for c in ['a', '1', 'x', 'y', '9', '0', 'j', '%', '+'] {
            assert!(!a_minus_b.contains(c), "set={}, c={}", a_minus_b, c);
        }

        assert!(CharSet::new().is_empty());
        assert!(CharSet::new().add_min('\0').complement().is_empty());
        assert_eq!(CharSet::new().complement().to_string(), "[..]");

        assert!(a.setminus(&a).is_empty());
    }
}
