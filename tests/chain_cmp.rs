use chain_cmp::chomp;

#[test]
fn test_macro() {
	assert!(chomp!(1 <= 2 < 3));
	assert!(!chomp!(1 <= 2 < 2));
	assert!(chomp!(1 <= 2 <= 2));
	assert!(chomp!(1 <= 2 + 2 < 5));
	assert!(chomp!(1 < 2 != 3));
	assert!(chomp!(2 != 3));

	let (a, b, c) = (1, 2, 3);
	assert!(chomp!(a < b < c));
	assert!(!chomp!(a > b > c));

	let (a, b, c) = (NoCopy(1), NoCopy(2), NoCopy(3));
	assert!(chomp!(a < b < c));
	assert!(!chomp!(a > b > c));
	assert!(!chomp!(a > b > c < a));

	assert!(chomp!(1 + 2 + 4 + 1 == 1 * 8));
	assert!(chomp!(0 == (100 < 10) as u8 == 0));
}

#[test]
fn test_short_circuit() {
	fn return_nr() -> i32 {
		panic!();
	}

	assert!(!chomp!(2 < 1 < return_nr()));
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct NoCopy(u32);
