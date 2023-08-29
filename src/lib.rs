// the PhantomData instances in this file are just to stop compiler complaints
// about missing generics; feel free to remove them

use std::{fmt::Display, ops::Rem};

/// A Matcher is a single rule of fizzbuzz: given a function on T, should
/// a word be substituted in? If yes, which word?
pub struct Matcher<'a, T> {
    substitution: Box<dyn Display + 'a>,
    matcher: Box<dyn Fn(T) -> bool + 'a>,
}

impl<'a, T> Matcher<'a, T> {
    pub fn new<F, S>(matcher: F, subs: S) -> Matcher<'a, T>
    where
        F: Fn(T) -> bool + 'a,
        S: Display + 'a,
    {
        Self {
            matcher: Box::new(matcher),
            substitution: Box::new(subs),
        }
    }
}

/// A Fizzy is a set of matchers, which may be applied to an iterator.
///
/// Strictly speaking, it's usually more idiomatic to use `iter.map()` than to
/// consume an iterator with an `apply` method. Given a Fizzy instance, it's
/// pretty straightforward to construct a closure which applies it to all
/// elements of the iterator. However, we're using the `apply` pattern
/// here because it's a simpler interface for students to implement.
///
/// Also, it's a good excuse to try out using impl trait.
pub struct Fizzy<'a, T: Display + Copy> {
    matchers: Vec<Matcher<'a, T>>,
}

impl<'a, T: Display + Copy> Default for Fizzy<'a, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, T: Display + Copy> Fizzy<'a, T> {
    pub fn new() -> Self {
        Self { matchers: vec![] }
    }

    // feel free to change the signature to `mut self` if you like
    #[must_use]
    pub fn add_matcher(mut self, matcher: Matcher<'a, T>) -> Self {
        self.matchers.push(matcher);
        self
    }

    /// map this fizzy onto every element of an iterator, returning a new iterator
    pub fn apply<I>(self, iter: I) -> impl Iterator<Item = String>
    where
        I: Iterator<Item = T>,
    {
        iter.map(|item| {
            let mut new = String::new();
            let mut matched = false;
            for matcher in &self.matchers {
                if (matcher.matcher)(item) {
                    matched = true;
                    new.push_str(matcher.substitution.to_string().as_str());
                }
            }
            if !matched {
                item.to_string()
            } else {
                new
            }
        })
    }
}

/// convenience function: return a Fizzy which applies the standard fizz-buzz rules
pub fn fizz_buzz<'a, T>() -> Fizzy<'a, T>
where
    T: Copy + Display + Rem<Output = T> + From<u8> + PartialEq,
{
    let new = Fizzy::new();

    // new.add_matcher(Matcher::new(|n: T| n % T::from(3) == T::from(0), "fizz"))
    //     .add_matcher(Matcher::new(|n: T| n % T::from(5) == T::from(0), "buzz"))
    new
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! expect {
        () => {
            vec![
                "1", "2", "fizz", "4", "buzz", "fizz", "7", "8", "fizz", "buzz", "11", "fizz",
                "13", "14", "fizzbuzz", "16",
            ]
        };
    }

    #[test]
    fn test_simple() {
        let got = fizz_buzz::<i32>().apply(1..=16).collect::<Vec<_>>();
        assert_eq!(expect!(), got);
    }

    #[test]
    fn test_u8() {
        let got = fizz_buzz::<u8>().apply(1_u8..=16).collect::<Vec<_>>();
        assert_eq!(expect!(), got);
    }

    #[test]
    fn test_u64() {
        let got = fizz_buzz::<u64>().apply(1_u64..=16).collect::<Vec<_>>();
        assert_eq!(expect!(), got);
    }

    #[test]
    fn test_nonsequential() {
        let collatz_12 = &[12, 6, 3, 10, 5, 16, 8, 4, 2, 1];
        let expect = vec![
            "fizz", "fizz", "fizz", "buzz", "buzz", "16", "8", "4", "2", "1",
        ];
        let got = fizz_buzz::<i32>()
            .apply(collatz_12.iter().cloned())
            .collect::<Vec<_>>();
        assert_eq!(expect, got);
    }

    #[test]
    fn test_custom() {
        let expect = vec![
            "1", "2", "Fizz", "4", "Buzz", "Fizz", "Bam", "8", "Fizz", "Buzz", "11", "Fizz", "13",
            "Bam", "BuzzFizz", "16",
        ];
        let fizzer: Fizzy<i32> = Fizzy::new()
            .add_matcher(Matcher::new(|n: i32| n % 5 == 0, "Buzz"))
            .add_matcher(Matcher::new(|n: i32| n % 3 == 0, "Fizz"))
            .add_matcher(Matcher::new(|n: i32| n % 7 == 0, "Bam"));
        let got = fizzer.apply(1..=16).collect::<Vec<_>>();
        assert_eq!(expect, got);
    }

    #[test]
    fn test_f64() {
        // a tiny bit more complicated becuase range isn't natively implemented on floats
        // NOTE: this test depends on a language feature introduced in Rust 1.34. If you
        // have an older compiler, upgrade. If you have an older compiler and cannot upgrade,
        // feel free to ignore this test.
        let got = fizz_buzz::<f64>()
            .apply(std::iter::successors(Some(1.0), |prev| Some(prev + 1.0)))
            .take(16)
            .collect::<Vec<_>>();
        assert_eq!(expect!(), got);
    }

    #[test]
    fn test_minimal_generic_bounds() {
        // NOTE: this test depends on a language feature introduced in Rust 1.34. If you
        // have an older compiler, upgrade. If you have an older compiler and cannot upgrade,
        // feel free to ignore this test.
        use std::fmt;
        use std::ops::{Add, Rem};
        #[derive(Clone, Copy, Debug, Default, PartialEq)]
        struct Fizzable(u8);
        impl From<u8> for Fizzable {
            fn from(i: u8) -> Fizzable {
                Fizzable(i)
            }
        }
        impl fmt::Display for Fizzable {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let Fizzable(ref n) = self;
                write!(f, "{n}")
            }
        }
        impl Add for Fizzable {
            type Output = Fizzable;
            fn add(self, rhs: Fizzable) -> Fizzable {
                let Fizzable(n1) = self;
                let Fizzable(n2) = rhs;
                Fizzable(n1 + n2)
            }
        }
        impl Rem for Fizzable {
            type Output = Fizzable;
            fn rem(self, rhs: Fizzable) -> Fizzable {
                let Fizzable(n1) = self;
                let Fizzable(n2) = rhs;
                Fizzable(n1 % n2)
            }
        }
        let got = fizz_buzz::<Fizzable>()
            .apply(std::iter::successors(Some(Fizzable(1)), |prev| {
                Some(*prev + 1.into())
            }))
            .take(16)
            .collect::<Vec<_>>();
        assert_eq!(expect!(), got);
    }
}
