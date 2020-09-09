use chain_cmp::chmp;

#[test]
fn test_macro() {
	assert!(chmp!(1 <= 2 < 3));
	assert!(!chmp!(1 <= 2 < 2));
	assert!(chmp!(1 <= 2 <= 2));
	assert!(chmp!(1 <= 2 + 2 < 5));
	assert!(chmp!(1 < 2 != 3));
	assert!(chmp!(2 != 3));

	let (a, b, c) = (1, 2, 3);
	assert!(chmp!(a < b < c));
	assert!(!chmp!(a > b > c));

	let (a, b, c) = (NoCopy(1), NoCopy(2), NoCopy(3));
	assert!(chmp!(a < b < c));
	assert!(!chmp!(a > b > c));
	assert!(!chmp!(a > b > c < a));

	assert!(chmp!(1 + 2 + 4 + 1 + 1 > 8));
	assert!(chmp!(0 == (100 < 10) as u8 == 0));
}

#[test]
fn test_short_circuit() {
	fn panics() -> i32 {
		panic!();
	}
	// This won't panic, because 2 < 1 will short-circuit the comparison chain
	assert!(!chmp!(2 < 1 < panics()));
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct NoCopy(u32);
