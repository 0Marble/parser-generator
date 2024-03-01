#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CharSet {
    ranges: Vec<(u32, u32)>,
}

impl CharSet {
    pub fn empty() -> Self {
        Self { ranges: vec![] }
    }

    pub fn add_range<A: Into<u32>>(&mut self, min: A, max: A) {
        let mut min = min.into();
        let mut max = max.into();

        if self.ranges.is_empty() {
            self.ranges.push((min, max));
            return;
        }

        let ranges = std::mem::take(&mut self.ranges);
        let mut put = false;
        for (a, b) in ranges {
            assert!(a <= b);
            assert!(min <= max);

            let overtaken = max != u32::MAX && a > max + 1;
            let not_reached = b + 1 < min;

            if overtaken && !put {
                put = true;
                self.ranges.push((min, max));
            }

            if overtaken || not_reached {
                self.ranges.push((a, b));
                continue;
            }

            min = min.min(a);
            max = max.max(b);
        }

        if !put {
            self.ranges.push((min, max));
        }
    }

    pub fn ranges(&self) -> impl Iterator<Item = (u32, u32)> + '_ {
        self.ranges.iter().cloned()
    }

    pub fn union(&self, other: &Self) -> Self {
        let mut res = self.clone();
        for (a, b) in other.ranges() {
            res.add_range(a, b);
        }
        res
    }
    pub fn complement(&self) -> Self {
        if self.ranges.is_empty() {
            return Self {
                ranges: vec![(0, u32::MAX)],
            };
        }
        let mut r = vec![];
        let (a, mut b) = self.ranges[0];
        if a != 0 {
            r.push((0, a - 1));
        }

        for (c, d) in &self.ranges[1..] {
            assert!(b + 1 < *c);
            r.push((b + 1, c - 1));
            b = *d;
        }

        if b != u32::MAX {
            r.push((b + 1, u32::MAX));
        }

        Self { ranges: r }
    }
    pub fn intersect(&self, other: &Self) -> Self {
        let mut res = Self::empty();

        for (a, b) in self.ranges() {
            for (c, d) in other.ranges() {
                if b < c || a > d {
                    continue;
                }

                res.add_range(a.max(c), b.min(d));
            }
        }

        res
    }
    pub fn insert<A: Into<u32>>(&mut self, x: A) {
        let x = x.into();
        self.add_range(x, x);
    }
    pub fn contains<A: Into<u32>>(&self, x: A) -> bool {
        let x = x.into();
        for (a, b) in self.ranges() {
            if a <= x && b >= x {
                return true;
            }
        }
        false
    }
    pub fn remove<A: Into<u32>>(&mut self, x: A) {
        let x = x.into();
        let ranges = std::mem::take(&mut self.ranges);
        for (a, b) in ranges {
            assert!(a <= b);
            if a > x || b < x {
                self.ranges.push((a, b));
                continue;
            }

            if a < x && b > x {
                self.ranges.push((a, x - 1));
                self.ranges.push((x + 1, b));
            } else if a == x && b != x {
                self.ranges.push((x + 1, b));
            } else if a != x && b == x {
                self.ranges.push((a, x - 1));
            }
        }
    }

    pub fn range<A: Into<u32>>(min: A, max: A) -> Self {
        let min = min.into();
        let max = max.into();
        assert!(min <= max);
        Self {
            ranges: vec![(min, max)],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.ranges.is_empty()
    }
}

impl<A> FromIterator<A> for CharSet
where
    A: Into<u32>,
{
    fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self {
        let mut res = Self::empty();
        for x in iter.into_iter() {
            res.insert(x.into());
        }
        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_range() {
        let mut s = CharSet::empty();
        s.add_range('b', 'c');
        assert_eq!(Vec::from_iter(s.ranges()), vec![('b' as _, 'c' as _)]);
        s.add_range('e', 'g');
        assert_eq!(
            Vec::from_iter(s.ranges()),
            vec![('b' as _, 'c' as _), ('e' as _, 'g' as _)]
        );
        s.add_range('a', 'd');
        assert_eq!(Vec::from_iter(s.ranges()), vec![('a' as _, 'g' as _)]);
    }

    #[test]
    fn contains() {
        let chars = ['a', 'b', 'c', 'ф', 'у', 'a', 'b', 'c', '你'];
        let s = CharSet::from_iter(chars.clone());
        for c in chars {
            assert!(s.contains(c), "Set does not contain '{c}'??");
        }
        assert!(!s.contains(0u32));
        assert!(!s.contains(u32::MAX));
        assert!(!s.contains('x'));
    }

    #[test]
    fn add_remove() {
        let mut s = CharSet::range('a', 'z');
        assert_eq!(Vec::from_iter(s.ranges()), vec![('a' as _, 'z' as _)]);

        s.add_range('A', 'Z');
        assert_eq!(
            Vec::from_iter(s.ranges()),
            vec![('A' as _, 'Z' as _), ('a' as _, 'z' as _)]
        );

        s.remove('a');
        assert_eq!(
            Vec::from_iter(s.ranges()),
            vec![('A' as _, 'Z' as _), ('b' as _, 'z' as _)]
        );
        s.remove('q');
        assert_eq!(
            Vec::from_iter(s.ranges()),
            vec![
                ('A' as _, 'Z' as _),
                ('b' as _, 'p' as _),
                ('r' as _, 'z' as _)
            ]
        );

        s.remove('!');
        assert_eq!(
            Vec::from_iter(s.ranges()),
            vec![
                ('A' as _, 'Z' as _),
                ('b' as _, 'p' as _),
                ('r' as _, 'z' as _)
            ]
        );
    }

    #[test]
    fn set_ops() {
        let a = CharSet::from_iter(['a', 'b', 'c']);
        let b = CharSet::from_iter(['b', 'd', 'e']);

        let a_or_b = a.union(&b);
        assert_eq!(
            Vec::from_iter(a_or_b.ranges()),
            vec![('a'.into(), 'e'.into())]
        );

        let a_and_b = a.intersect(&b);
        assert_eq!(
            Vec::from_iter(a_and_b.ranges()),
            vec![('b'.into(), 'b'.into())]
        );

        let not_a = a.complement();
        assert_eq!(
            Vec::from_iter(not_a.ranges()),
            vec![(0, 'a' as u32 - 1), ('c' as u32 + 1, u32::MAX)]
        );
        let not_b = b.complement();
        assert_eq!(
            Vec::from_iter(not_b.ranges()),
            vec![
                (0, 'b' as u32 - 1),
                ('c' as u32, 'c' as u32),
                ('e' as u32 + 1, u32::MAX)
            ]
        );

        assert_eq!(CharSet::range(0, u32::MAX).complement(), CharSet::empty());
        assert_eq!(CharSet::range(0, u32::MAX), CharSet::empty().complement());
    }
}
