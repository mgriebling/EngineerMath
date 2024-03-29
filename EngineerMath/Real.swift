//
//  Real.swift
//  XNumbers
//
//  Created by Mike Griebling on 4 Mar 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

/*
Reals - Mathematical functions for an arbitrary-precision floating
point representation named Real.

Copyright (C) 1997-2004 Michael Griebling
From an original FORTRAN library MPFUN by David H. Bailey, NASA Ames
Research Center which is available from:

http://crd.lbl.gov/~dhbailey/mpdist/index.html

Source translated with permission of the original author.

This module is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This module is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/

/*
From the author's description:

The following information is a brief description of this program.  For
full details and instructions for usage, see the paper "A Portable High
Performance Multiprecision Package", available from the author.

This package of Fortran subroutines performs multiprecision floating point
arithmetic.  If sufficient main memory is available, the maximum precision
level is at least 16 million digits.  The maximum dynamic range is at
least 10^(+-14,000,000).  It employs advanced algorithms, including an
FFT-based multiplication routine [Not yet in Oberon-2--MG] and some recently discovered
quadratically convergent algorithms for pi, exp and log.  The package also
features extensive debug and self-checking facilities, so that it can be
used as a rigorous system integrity test.  All of the routines in this
package have been written to facilitate vector, parallel processing and/or
RISC processing.

My comments:

The current algorithms are not optimized for really large numbers.  if there
is some interest, I will be porting those algorithms as well from the original
author.  The existing algorithms will work in a reasonable time for most numbers
and are hard-limited via a constant `maxDigits' to a maximum of about 500 digits.
This constant can be easily increased.  Of course, performance will suffer.  The
actual working precision is adjustable via the `SetWords' routine.  The actual
number of words will { be reflected in the `curMantissa' read-only constant.
Each word gives about 7.22 digits of precision.  The default precision is set
to `maxDigits'.

There may be a couple of bugs in several routines which affect the accuracy
in the last few places of the result.  This shouldn't make much difference to
casual users as long as the precision is set to be 10-20 digits more than you
need.
*/

struct Real : Equatable, Hashable, CustomStringConvertible {
	
	private static let DEBUG  = false
	private static let HALF   = 0.5
	private static let invLn2 = 1.4426950408889633
	private static let Ln2    = 0.693147180559945309
	
	/* numeric precision-setting constants */
	static let maxDigits   = 520                                    /* initial precision level in digits */
	static let outDigits   = 56                                     /* initial output precision level in digits */
	static let log10eps    = 10-maxDigits                           /* log10 of initial eps level */
	static let digsPerWord = 7.224719896                            /* decimal digits per word (digit) */
	static let maxMant     = Int(Double(maxDigits)/digsPerWord+1.5) /* hardcoded maximum mantissa words */
	static let maxExp      = 2000000                                /* maximum exponent */
	
	/* internal scaling constants */
	private static let mpbbx    = 4096.0
	private static let radix    = mpbbx*mpbbx
	private static let mpbx2    = radix*radix
	private static let mprbx    = 1.0/mpbbx
	private static let invRadix = mprbx*mprbx
	private static let mprx2    = invRadix*invRadix
	private static let mprxx    = 16*mprx2
	
	/* miscellaneous constants */
	private static let NBT   = 24
	private static let NPR   = 32
	private static let MPIRD = 1
	private static let NIT   = 3
	
	enum Status {
		case Okay
		case Overflow
		case Underflow
		case DivideByZero
		case TooFewDigits
		case TooManyDigits
		case IllegalNumber
		case UndefinedStorage
		case IllegalOperator
		case MismatchBraces
		case IllegalArgument
		
		// Scanner errors
		case ExpectingRBrace
		case IllegalExpression
		case IllegalVariable
		case TooManyVariables
		case ExpectingName
		case ExpectingAssign
		case Undefined
		case ExpectingArgs
		case IncompatibleArgs
	}

	private typealias RealArray = [Double]
	private var real: RealArray

	private struct FixedLReal {
		var real = RealArray(repeating: 0, count:maxMant+8)
	}

	static var err : Int = 0
	static var debug : Int = 0
	
	private static var curMantissa : Int = Real.maxMant+1
	private static var numBits : Int = 22
	private static var sigDigs : Int = Real.maxDigits
	
	static var digits : Int {
		set {
			/** Sets the number of active words in all Real computations.
			One word contains about 7.22 digits. */
			var words = Double(newValue) / Real.digsPerWord
			if words < 8 { words = 8 }
			if words <= Double(Real.maxMant-2) {
				Real.curMantissa = Int(words)+2
				Real.sigDigs = digits
			}
		}
		get { Real.sigDigs }
	}
	
	// inself.ternal parameters
	private static var Pi = Zero
	private	static var Log2 = Zero
	private static let One = RealArray(arrayLiteral: 1, 0, 1, 0, 0, 0, 0, 0)
	private static let Zero = RealArray(arrayLiteral: 0, 0)
	
	public static let one = Real(1)
	public static let zero = Real(0)
    public static var pi : Real {
		if Pi.count == 0 {
			Initialize()
		}
		return Real(Pi)
	}
    public static var ln2 : Real {
		if Log2.count == 0 {
			Initialize()
		}
		return Real(Log2)
	}
	
	/* Speed up very large factorials */
	private static let f1 =
		"2.82422940796034787429342157802453551847749492609122485057891808654297795090106301787" +
		"2551771413831163610713611737361962951474996183123918022726073409093832422005556968866" +
		"7840380377379444961268380147875111966906386044926144538111370090160766866405407170565" +
		"9522612980419583567789090475415128711408369242515352930962606722710387442460886354543" +
		"6398293174776177553262185112647485586491818038151987716121968151412990230446382406889" +
		"65083575002296499396423642566352716149352078013312029433930594819960435395E+456573"
	private static let f2 =
		"1.42022534547031440496694633368230597608996535674640162269622474462922677851609968565" +
		"0082553407879081329793135215376044079156034995456792440298907698327157087066286303182" +
		"5017623219084061256114573810476379717993512721296946450311966946288603601628556916324" +
		"4648770389480378251602819955788158117868794159097393435551925337859488859955701890215" +
		"4897701489299055308898497995637308558323762472340297297985768615383843817767617482336" +
		"58088832083067784773860727948019819421544453708479108922842308732119367523E+973350"
	private static let f3 =
		"1.47739153173803909429290747493561414549932051952374408795791384376505240135170347653" +
		"2418899010198829649964892384917975071774129347530818714855332590431274389350896312260" +
		"9806517049255450392732030550644905383028447932954677114843634677423190476154873121734" +
		"1025709069449617692835058182617595979172730842885422104493186754451133578337885783639" +
		"5817086347597543562761254468984063083893218681681196080370667835191599919282226318984" +
		"62208531038106191099127491142755685344624042273747482199422127053615182013E+1512851"
	private static let fact100000 = Real(f1)
	private static let fact200000 = Real(f2)
	private static let fact300000 = Real(f3)
	
	private static var Seed = Real(4)
    public static var status = Status.Okay
	
    public var description: String {
		return self.toString()
	}
	
	/*---------------------------------------------------------*/
	/* Constructors                                            */
	
	init (size: Int) {
		self.real = RealArray(repeating: 0, count: size)
	}
	
	init (_ string: String) {
		self.init(size: 0)
        self = Real.fromString(string)
	}
	
	init (_ integer: Int) {
		self.init(Double(integer))
	}
	
	init (_ double: Double) {
		/* create a new number */
		self.init(size:Real.curMantissa+4)
        Real.NumbExpToReal(double, n: 0, b: &self.real)
	}
	
	private init (_ fromArray: RealArray) {
		self.init(size:Real.curMantissa+4)
        Real.copy(fromArray, b:&self.real)		/* b = a */
	}

	init (_ real: Real) {
		/** return a copy of \e real. */
		self.init(real.real)
	}

	/*---------------------------------------------------------*/
	/* Internal basic operator definitions                     */
	
	private static func Min (_ x: Int, y: Int) -> Int {
		if x<y { return x } else { return y }
	} //Min;
	
	private static func Max (_ x: Int, y: Int) -> Int {
		if x>y { return x } else { return y }
	} //Max;
	
	private static func Sign (_ x: Int, y: Double) -> Int {
		if y < 0 {
			return -Int(x)
		} else {
			return Int(x)
		}
	} //Sign;
	
	private static func Zero (_ x: inout RealArray) {
		/* x to zero */
		x[0] = 0; x[1] = 0
	} //Zero;
	
	private static func ToInt (_ x: Double) -> Int {
		return Int(x)
	} //ToInt;
	
	private static func ODD (_ x: Int) -> Bool {
		return (x&1) != 0
	}

	private static func ipower (_ x: Double, base: Int) -> Double {
		/* ipower(x, base) returns the x to the integer power base where base*Log2(x) < Log2(Max) */
		var y: Double
		var neg: Bool
		var ibase = base
		var ix = x
		
		/* compute x**base using an optimised algorithm from Knuth, slightly
		altered : p442, The Art Of Computer Programming, Vol 2 */
		y = 1; if ibase<0 { neg = true; ibase  =  -ibase } else { neg = false }
		while true {
			if ODD(ibase) { y = y*ix };
			ibase = ibase / 2; if ibase==0 { break };
			ix = ix*ix
		}
		if neg { return 1.0/y } else { return y }
	} //ipower;
	
	private static func Reduce (_ a: inout Double, exp: inout Int) {
		/* reduce `a' to be within 1 and radix and adjust
		the exponent `exp' appropriately */
		let maxIterations=100
		
		if a>=Real.radix {
			for k in 1...maxIterations {
				a = Real.invRadix*a;
				if a<Real.radix { exp += k; return }
			}
		} else if a<1.0 {
			for k in 1...maxIterations {
				a = Real.radix*a;
				if a>=1.0 { exp -= k; return }
			}
		}
	} //Reduce;


	private static func copy (_ a: RealArray, b: inout RealArray) {
		/* b = a */
		var ia, na: Int
		ia = Sign(1, y: a[0]); na = Min(Int(abs(a[0])), y: Real.curMantissa)
		if na == 0 { Zero(&b); return }
		b[0] = Double(Sign(na, y:Double(ia)))
		for i in 1...na+2 { b[i] = a[i] }
		ia += 1
	} //copy;
	
	private static func Write (_ q: RealArray) {
		/* output raw number */
		for i in 0...Int(abs(q[0]))+1 {
			print("x[\(i)]=\(Int(q[i]))")
		}
		print()
	} //Write;

	private static func Round (_ a: inout RealArray) {
		/*
		This performs rounding and truncation of the a number.
		The maxExp value is the absolute value of the largest exp1nt
		word allowed for extended numbers.
		*/
		var a2: Double
		var allZeros: Bool
		var ia, na, n4, i, k: Int
		
		/* error testing */
		if Real.err != 0 { Zero(&a); return }
	
		/* check for initial zeros */
		a2 = a[1]; a[1] = 0; ia = Sign(1, y: a[0])
		na = Min(Int(abs(a[0])), y: Real.curMantissa)
		n4 = na+4; k = 0
		if a[2] == 0 {
			/* find the nonzero word and shift the entire number left.
			The length of the result is reduced by the length of the
			shift */
			allZeros = true; i = 4
			while allZeros && (i <= n4) {
				if a[i-1] != 0 { allZeros = false; k = i-3 }
				i += 1
			}
			if allZeros { Zero(&a); return }
			for i in 2...n4-k-1 { a[i] = a[i+k] }
			a2 = a2-Double(k); na = na-Max(k-2, y:0)
		}
		
		/* perform rounding depending on MPIRD */
		if (na == Real.curMantissa) && (Real.MPIRD >= 1) {
			if ((Real.MPIRD == 1) && (a[na+2] >= (Real.HALF*Real.radix))) || ((Real.MPIRD == 2) && (a[na+2] >= 1)) {
				a[na+1] = a[na+1] + 1
			}
			
			/* release carries as far as necessary due to rounding */
			i = na+1
			while true {
				if i < 2 { a[2] = a[1]; na = 1; a2 = a2 + 1; break }
				if a[i] < Real.radix { break }
				a[i] = a[i]-Real.radix; a[i-1] = a[i-1] + 1
				i -= 1
			}
		}
		
		/* At least the last mantissa word is zero.  Find the last
		nonzero word and adjust the length of the result accordingly */
		if a[na+1] == 0 {
			i = na+2
			while i >= 3 {
				if a[i-1] != 0 { na = i-2; i = 1 }
				i -= 1
			};
			if i != 0 { Zero(&a); return }
		}
		
		/* check for overflow and underflow */
		if a2 < -Double(Real.maxExp) {
			print("*** Round: Exponent underflow!")
			Real.err = 68
		} else if a2 > Double(Real.maxExp) {
			print("*** Round: Exponent overflow!")
			Real.err = 69
		}
		
		/* check for zero */
		if a[2] == 0.0 {
			Zero(&a)
		} else {
			a[0] = Double(Sign(na, y:Double(ia))); a[1] = a2
			a[na+2] = 0.0; a[na+3] = 0.0
		}
	} //Round;

	private static func Normalize (_ d: RealArray, a: inout RealArray) {
		/*
		This converts the number in array d to the standard normalized
		form in a.  Values in d are often negative or exceed the maximum
		radix radix in result arrays, and this fixes them.
		
		Normalize assumes that two extra mantissa words are input at the
		end of d.  This reduces precision loss when it is necessary to
		shift the result to the left.  The output is placed in the array
		a.  Debug output starts with debug = 10.
		*/
		var a2: Double
		var t1, t2, t3: Double
		var ia, na, n4: Int
        var d = d
		
		if Real.err != 0 { Zero(&a); return }
		ia = Sign(1, y:d[0]); na = Min(Int(abs(d[0])), y:Real.curMantissa)
		if na == 0 { Zero(&a); return }
		n4 = na+4; a2 = d[1]; d[1] = 0
		while true {
			t1 = 0
			for i in stride(from: n4-1, through: 2, by: -1) {
				t3 = t1+d[i]; t2 = Real.invRadix*t3; t1 = Double(Int(t2))
				if (t2 < 0) && (t1 != t2) { t1 = t1-1 }
				d[i] = t3-t1*Real.radix
			}
			d[1] = d[1]+t1;
			if d[1] < 0 {
				/* negate all words and re-normalize */
				ia = -ia; d[2] = d[2]+Real.radix*d[1]; d[1] = 0
				for i in 1...n4-1 { d[i] = -d[i] }
			} else if d[1] > 0 {
				/* nonzero number spilled into d[1].  Shift the entire number
				right one cell.  The exponent and length of the result are
				increased by one. */
				for i in stride(from: n4-1, through: 2, by: -1) { a[i] = d[i-1] }
				na = Min(na+1, y:Real.curMantissa); a2 = a2+1.0
				break
			} else {
				for i in 2...n4-1 { a[i] = d[i] }
				break
			}
		}
		
		/* perform rounding and truncation */
		a[0] = Double(Sign(na, y:Double(ia))); a[1] = a2
		Round(&a)
	} //Normalize;

	private static func RealToNumbExp (_ a: RealArray, b: inout Double, n: inout Int) {
		/*
		This routine converts the multiprecision number `a' to the number
		`d'*2**`n', accurate to between 14-17 digits, dep}ing on the
		system.  `b' will be between 1 and radix.
		*/
		var aa: Double
		var na: Int
		
		/* handle error propogation */
		if Real.err != 0 { b = 0; n = 0; return }
		
		/* trivial cases */
		if a[0] == 0 { b = 0; n = 0; return }
		
		/* real algorithm */
		na = Int(abs(a[0])); aa = a[2]
		if na >= 2 { aa = aa+Real.invRadix*a[3] }
		if na >= 3 { aa = aa+Real.mprx2*a[4] }
		if na >= 4 { aa = aa+Real.invRadix*Real.mprx2*a[5] }
		
		n = Real.NBT*Int(a[1])
		if a[0] < 0 { b = -aa } else { b = aa }
	} //RealToNumbExp;

	private static func NumbExpToReal (_ a: Double, n: Int, b: inout RealArray) {
		/*
		This routine converts the number `a'*2**`n' to an extended form
		in `b'.  All bits of `a' are recovered in `b'.  However, note
		for example that if `a'=0.1D0 and `n'=0, { `b' will NOT be
		the multiprecision equivalent of 1/10.  Debug output starts
		with debug = 9.  Pre: LEN(b)>=8.
		*/
		var aa: Double
		var n1, n2: Int
		
		assert(b.count >= 8, "Assertion NumbExptoReal")
		
		/* check for zero */
		if a == 0 { Zero(&b); return }
		n1 = Int(n/Real.NBT); n2 = n-Real.NBT*n1
		aa = abs(a)*ipower(2.0, base: n2)
		
		/* reduce aa to within 1 and radix */
		Reduce(&aa, exp: &n1)
		
		/* store successive sections of aa into b */
		b[1] = Double(n1)
		b[2] = Double(Int(aa)); aa = Real.radix*(aa-b[2])
		b[3] = Double(Int(aa)); aa = Real.radix*(aa-b[3])
		b[4] = Double(Int(aa)); aa = Real.radix*(aa-b[4])
		b[5] = Double(Int(aa))
		b[6] = 0
		b[7] = 0
		
		/* find length of resultant number */
		for i in stride(from: 5, through: 2, by: -1) {
			if b[i] != 0 { b[0] = Double(Sign(i-1, y:a)); return }
		}
		b[0] = 0
	} //NumbExpToReal;

	private static func Add (_ c: inout RealArray, a: RealArray, b : RealArray) {
		/*
		This routine adds MP numbers a and b to yield the MP
		sum c.  It attempts to include all significance of a
		and b in the result, up to the maximum mantissa length
		curMantissa.  Debug output starts with debug = 9.
		*/
		var ia, ib, na, nb, nsh: Int
		var ixa, ixb, ixd, ish, m1, m2, m3, m4, m5, nd: Int
		var db: Double
		var d = Real(size: Real.curMantissa+6)
		
		if Real.err != 0 { Zero(&c); return }
		if Real.debug >= 9 {
			print("Add 1"); Write(a)
			print("Add 2"); Write(b)
		}
		ia = Sign(1, y: a[0]); ib = Sign(1, y: b[0])
		na = Min(Int(abs(a[0])), y: Real.curMantissa)
		nb = Min(Int(abs(b[0])), y: Real.curMantissa)
		
		/* check for zero inputs */
		if na == 0 { /* a is zero -- the result is b */
			c[0] = Double(Sign(nb, y:Double(ib)))
            for i in 1...nb+1 { c[i] = b[i] }
		} else if nb == 0 { /* b is zero -- the result is a */
			c[0] = Double(Sign(na, y:Double(ia)))
            for i in 1...na+1 { c[i] = a[i] }
		} else {
			if ia == ib { db = 1.0 } else { db = -1.0 }
			ixa = Int(a[1]); ixb = Int(b[1]); ish = ixa-ixb
			
			/* check if `a's exponent is greater than `b's */
			if ish >= 0 {
				/* `b' must be shifted to the right */
				m1 = Min(na, y: ish); m2 = Min(na, y: nb+ish); m3 = na
				m4 = Min(Max(na, y: ish), y: Real.curMantissa+1)
				m5 = Min(Max(na, y: nb+ish), y: Real.curMantissa+1)
				d.real[0] = 0; d.real[1] = 0
                for i in stride(from: 1, through: m1, by: 1)    { d.real[i+1] = a[i+1] }
                for i in stride(from: m1+1, through: m2, by: 1) { d.real[i+1] = a[i+1]+db*b[i+1-ish] }
                for i in stride(from: m2+1, through: m3, by: 1) { d.real[i+1] = a[i+1] }
                for i in stride(from: m3+1, through: m4, by: 1) { d.real[i+1] = 0 }
                for i in stride(from: m4+1, through: m5, by: 1) { d.real[i+1] = db*b[i+1-ish] }
				nd = m5; ixd = ixa; d.real[nd+2] = 0; d.real[nd+3] = 0
			} else {
				/* `b' has greater exponent than `a', so `a' is shifted
				to the right. */
				nsh = -ish; m1 = Min(nb, y:nsh); m2 = Min(nb, y:na+nsh); m3 = nb
				m4 = Min(Max(nb, y:nsh), y:Real.curMantissa+1)
				m5 = Min(Max(nb, y:na+nsh), y:Real.curMantissa+1)
				d.real[0] = 0; d.real[1] = 0
                for i in stride(from: 1, through: m1, by: 1)    { d.real[i+1] = db*b[i+1] }
                for i in stride(from: m1+1, through: m2, by: 1) { d.real[i+1] = a[i+1-nsh]+db*b[i+1] }
                for i in stride(from: m2+1, through: m3, by: 1) { d.real[i+1] = db*b[i+1] }
                for i in stride(from: m3+1, through: m4, by: 1) { d.real[i+1] = 0 }
                for i in stride(from: m4+1, through: m5, by: 1) { d.real[i+1] = a[i+1-nsh] }
				nd = m5; ixd = ixb; d.real[nd+2] = 0; d.real[nd+3] = 0
			}
			d.real[0] = Double(Sign(nd, y:Double(ia))); d.real[1] = Double(ixd)
			Normalize(d.real, a:&c)
		}
		if Real.debug >= 9 { print("Add 3"); Write(c) }
	} //Add;

	private static func Sub (_ c: inout RealArray, a: RealArray, b: RealArray) {
		if Real.err != 0 { Zero(&c); return }
        var b = b
		
		/* negate and perform addition */
		b[0] = -b[0]; Add(&c, a: a, b: b)
	} //Sub;

	private static func Mul (_ c: inout RealArray, a: RealArray, b: RealArray) {
		/*
		This routine multiplies numbers `a' and `b' to yield the
		product `c'.  When one of the arguments has a much higher
		level of precision than the other, this routine is slightly
		more efficient if `a' has the lower level of precision.
		Debug output starts with debug = 8.
		
		This routine returns up to curMantissa mantissa words of the
		product.  if the complete double-long product of `a' and
		`b' is desired, then curMantissa must be at least as large as
		the sum of the mantissa lengths of `a' and `b'.  In other
		words, if the precision levels of `a' and `b' are both
		64 words, then curMantissa must be at least 128 words to
		obtain the complete double-long product in `c'.
		*/
		var ia, ib, na, nb, nc, i1, i2, n2, j3: Int
		var d2, t1, t2: Double
		var d = FixedLReal() // Real(size: 2*Real.curMantissa+6)
		
		if Real.err != 0 { Zero(&c); return }
		if Real.debug >= 8 {
			print("Mul 1 "); Write(a)
			print("Mul 2 "); Write(b)
		}
		ia = Sign(1, y: a[0]); ib = Sign(1, y: b[0])
		na = Min(Int(abs(a[0])), y:Real.curMantissa)
		nb = Min(Int(abs(b[0])), y:Real.curMantissa)
		
		/* if one of the inputs is zero--result is zero */
		if (na == 0) || (nb == 0) { Zero(&c); return }
		
		/* check for multiplication by 1 */
		if (na == 1) && (a[2] == 1) {
			/* a is 1 or -1 -- result is b or -b */
			c[0] = Double(Sign(nb, y:Double(ia*ib))); c[1] = a[1]+b[1]
            for i in 2...nb+1 { c[i] = b[i] }
			return
		} else if (nb == 1) && (b[2] == 1) {
			/* b is 1 or -1 -- result is a or -a */
			c[0] = Double(Sign(na, y:Double(ia*ib))); c[1] = a[1]+b[1]
            for i in 2...na+1 { c[i] = a[i] }
			return
		}
		
		nc = Min(na+nb, y: Real.curMantissa);
		d2 = a[1]+b[1];
        for i in 0...nc+3 { d.real[i] = 0 }
		
		/* perform ordinary long multiplication algorithm.
		Accumulate at most curMantissa+4 mantissa words of the
		product. */
        for j in 3...na+2 {
			t1 = a[j-1]; j3 = j-3;
			n2 = Min(nb+2, y:Real.curMantissa+4-j3)
            for i in 2...n2-1 {
				d.real[i+j3] = d.real[i+j3]+t1*b[i]
			}
			
			/* release carries periodically to avoid overflowing
			the exact integer capacity of double precision
			floating point words in d */
			if ((j-2) % Real.NPR) == 0 {
				i1 = Max(3, y: j-Real.NPR); i2 = n2+j3
                for i in i1...i2 {
					t1 = d.real[i-1]; t2 = Double(Int(Real.invRadix*t1))
					d.real[i-1] = t1-Real.radix*t2
					d.real[i-2] = d.real[i-2]+t2
				}
			}
		};
		
		/* if d[1] is nonzero, shift the result one cell right */
		if d.real[1] != 0 {
			d2 = d2+1
			for i in stride(from: nc+3, through: 2, by: -1) { d.real[i] = d.real[i-1] }
		}
		d.real[0] = Double(Sign(nc, y:Double(ia*ib))); d.real[1] = d2
		
		/* fix up result since some words may be negative or
		exceed radix */
		Normalize(d.real, a: &c)
		if Real.debug >= 9 { print("Mul 3 "); Write(c) }
	} //Mul;

	private static func Muld (_ c: inout RealArray, a: RealArray, b: Double, n: Int) {
		/*
		This routine multiplies the multiple precision number `a'
		by the number `b'*2**`n' to produce the multiple precision
		result in `c'.
		*/
		var bb: Double
		var d = Real(size: Real.curMantissa+4)
		var ia, ib, n1, n2, na: Int
        var f = RealArray(repeating:0, count:3)
		
		if Real.err != 0 { Zero(&c); return }
		if Real.debug >= 9 {
			print("Muld 1"); Write(a)
			print("Muld 2 \(b); n=\(n)")
		}
		
		/* check for zero inputs */
		ia = Sign(1, y:a[0]); ib = Sign(1, y:b)
		na = Min(Int(abs(a[0])), y:Real.curMantissa)
		if (na == 0) || (b == 0) { Zero(&c); return }
		n1 = Int(n/Real.NBT); n2 = n-Real.NBT*n1; bb = abs(b)*ipower(2.0, base: n2)
		
		/* reduce bb to within 1 and radix */
		Reduce(&bb, exp: &n1)
		
		/* if `b' cannot be represented exactly in a single mantissa word use Mul */
		if bb != Double(Int(bb)) {
			if b < 0 { bb = -abs(b) } else { bb = abs(b) }
			NumbExpToReal(bb, n: n1*Int(Real.NBT), b: &f)  /* convert bb to f (Real) */
			Mul(&c, a: f, b: a)
		} else {
			/* perform short multiply */
            for i in 2...na+1 { d.real[i] = bb*a[i] }
			
			/* set exponent and fix up the result */
			d.real[0] = Double(Sign(na, y: Double(ia*ib))); d.real[1] = a[1]+Double(n1)
			d.real[na+2] = 0; d.real[na+3] = 0
			Normalize(d.real, a: &c)
		}
		
		if Real.debug >= 9 {
			print("Muld 3"); Write(c)
		}
	} //Muld;

	private static func Div (_ c: inout RealArray, a: RealArray, b: RealArray) {
		/*
		This routine divides the number `a' by the number `b' to yield
		the quotient `c'.  Debug output starts with debug = 8.
		*/
		var ia, ib, na, nb, nc, i3, i2, j, j3, md, is1, ij: Int
		var rb, ss, t0, t1, t2: Double
		var useOldj: Bool
		var d = FixedLReal() // Real(size: Real.curMantissa+4)
		
		/* handle errors */
		if Real.err != 0 { Zero(&c); return }
		if Real.debug >= 8 {
			print("Div 1"); Write(a)
			print("Div 2"); Write(b)
		}
		
		/* extract lengths and number signs */
		ia = Sign(1, y:a[0]); ib = Sign(1, y:b[0])
		na = Min(Int(abs(a[0])), y:Real.curMantissa)
		nb = Min(Int(abs(b[0])), y:Real.curMantissa)
		
		/* check if divid} //is zero */
		if na == 0 { Zero(&c); return };
		
		/* check for divisors of 1 or -1 */
		if (nb == 1) && (b[2] == 1) {
			c[0] = Double(Sign(na, y: Double(ia*ib)))
			c[1] = a[1]-b[1]
			for i in 2...na+1 { c[i] = a[i] }
			return
		}
		
		/* check if divisor is zero */
		if nb == 0 {
			print("*** Div: Divisor is zero!")
			Real.err = 31; return
		}
		
		/* initialize trial divisor and trial division */
		t0 = Real.radix*b[2]
		if nb>=2 { t0 = t0+b[3] }
		if nb>=3 { t0 = t0+Real.invRadix*b[4] }
		if nb>=4 { t0 = t0+Real.mprx2*b[5] }
		rb = 1.0/t0; md = Min(na+nb, y: Real.curMantissa)
		d.real[0] = 0
		for i in 1...na { d.real[i] = a[i+1] }
        for i in na+1...md+3 { d.real[i] = 0 }
		
		/* perform ordinary long division algorithm.  First
		compute only the first na words of the quotient. */
        for j in 2...na+1 {
			t1 = Real.mpbx2*d.real[j-2]+Real.radix*d.real[j-1]+d.real[j]+Real.invRadix*d.real[j+1]
			t0 = Double(Int(rb*t1)); j3 = j-3
			i2 = Min(nb, y:Real.curMantissa+2-j3)+2
			ij = i2+j3
            for i in 3...i2 {
				i3 = i+j3-1; d.real[i3] = d.real[i3]-t0*b[i-1]
			}
			
			/* release carries periodically to avoid overflowing
			the exact integer capacity of double precision
			floating point words in d. */
			if j-1 % Real.NPR == 0 {
                for i in j...ij-1 {
					t1 = d.real[i]; t2 = Double(Int(Real.invRadix*t1))
					d.real[i] = t1-Real.radix*t2; d.real[i-1] = d.real[i-1]+t2
				}
			};
			d.real[j-1] = d.real[j-1]+Real.radix*d.real[j-2]
			d.real[j-2] = t0
		}
		
		/* compute additional words of the quotient, as long as
		the remainder is nonzero. */
		j = na+2; useOldj = false
		while true {
			if j>Real.curMantissa+3 { break }
			t1 = Real.mpbx2*d.real[j-2] + Real.radix*d.real[j-1] + d.real[j]
			if j <= Real.curMantissa+2 { t1 = t1+Real.invRadix*d.real[j+1] }
			t0 = Double(Int(rb*t1)); j3 = j-3
			i2 = Min(nb, y:Real.curMantissa+2-j3)+2
			ij = i2+j3; ss = 0
			
			for i in 3...i2 {
				i3 = i+j3-1; d.real[i3] = d.real[i3]-t0*b[i-1];
				ss = ss+abs(d.real[i3])
			}
			
			if j-1 % Real.NPR == 0 {
				for i in j...ij-1 {
					t1 = d.real[i]; t2 = Double(Int(Real.invRadix*t1))
					d.real[i] = t1-Real.radix*t2; d.real[i-1] = d.real[i-1]+t2
				}
			}
			d.real[j-1] = d.real[j-1]+Real.radix*d.real[j-2]
			d.real[j-2] = t0
			if ss == 0 { useOldj = true; break }
			if ij <= Real.curMantissa { d.real[ij+2] = 0 }
			j += 1
		}
		
		/* set sign and exponent, and fix up result */
		if !useOldj { j = Real.curMantissa+3 }
		d.real[j-1] = 0
		if d.real[0] == 0 { is1 = 1 } else { is1 = 2 }
		nc = Min(j-1, y:Real.curMantissa);
		d.real[nc+2] = 0; d.real[nc+3] = 0
		for i in stride(from: j, through: 2, by: -1) { d.real[i] = d.real[i-is1] }
		d.real[0] = Double(Sign(nc, y:Double(ia*ib)))
		d.real[1] = a[1]-b[1]+Double(is1-2)
		Normalize(d.real, a:&c)
		
		if Real.debug >= 8 {
			print("Div 3"); Write(c)
		};
	} //Div;

	private static func Divd (_ c: inout RealArray, a: RealArray, b: Double, n: Int) {
		/*
		This routine divides the multiple precision number `a'
		by the number `b'*2**`n' to produce the multiple precision
		result in `c'.
		*/
		var t1, bb, br, dd: Double
		var d = Real(size: Real.curMantissa+4)
		var ia, ib, n1, n2, nc, na, j: Int
		var ok: Bool
        var f = RealArray(repeating: 0, count:3)
		
		if Real.err != 0 { Zero(&c); return }
		ia = Sign(1, y:a[0]); ib = Sign(1, y:b)
		na = Min(Int(abs(a[0])), y: Real.curMantissa)
		
		/* check if divid} //is zero */
		if na == 0 { Zero(&c); return }
		
		/* check if divisor is zero */
		if b == 0 {
			print("*** Divd: Divisor is zero!")
			Real.err = 32; return
		}
		
		n1 = Int(n/Real.NBT); n2 = n-Real.NBT*n1; bb = abs(b)*ipower(2.0, base: n2)
		
		/* reduce bb to within 1 and radix */
		Reduce(&bb, exp:&n1)
		
		/* if `b' cannot be represented exactly in a single mantissa word { use Div */
		if bb != Double(Int(bb)) {
			if b < 0 { bb = -abs(b) } else { bb = abs(b) }
			NumbExpToReal(bb, n:n1*Real.NBT, b:&f)  /* convert bb to f (Real) */
			Div(&c, a: a, b: f)
		} else {
			/* perform short division */
			br = 1.0/bb; dd = a[2]
			j = 2; ok = true
			while ok && (j <= Real.curMantissa+3) {
				t1 = Double(Int(br*dd)); d.real[j] = t1
				dd = Real.radix*(dd-t1*bb)
				if j <= na {
					dd = dd+a[j+1]
				} else if dd == 0 {
					ok = false
				}
				j += 1
			}
			
			/* set exponent and fix up the result */
			j -= 1; nc = Min(j-1, y:Real.curMantissa);
			d.real[0] = Double(Sign(nc, y:Double(ia*ib))); d.real[1] = a[1]-Double(n1)
			if j<=Real.curMantissa+2 { d.real[j+1] = 0 }
			if j<=Real.curMantissa+1 { d.real[j+2] = 0 }
			Normalize(d.real, a: &c)
		}
	} //Divd;

	private static func Abs (_ z: inout RealArray, x: RealArray) {
		copy(x, b: &z); z[0] = abs(x[0])
	} //Abs;

	private static func IntPower (_ b: inout RealArray, a: RealArray, n: Int) {
		/*
		This routine computes the `n'-th power of the extended
		number `a' and returns the extended result in `b'.
		When `n' is negative, the reciprocal of `a'**|`n'| is
		returned.  The Knuth's method is used p442, "The Art
		of Computer Programming", Vol 2.
		*/
		var na, nws, nn: Int
		var r = FixedLReal() // Real(size: Real.curMantissa+4)
		var t = FixedLReal() // Real(size: Real.curMantissa+4)
		
		if Real.err != 0 { Zero(&b); return }
		na = Min(Int(abs(a[0])), y:Real.curMantissa)
		
		/* check for errors */
		if na == 0 {
			if n >= 0 {
				Zero(&b)
			} else {
				print("*** ipower: Argument is zero and n is <= 0.")
				Real.err = 57
			}
			return
		}
		
		/* check for trival cases */
		Zero(&t.real)
		nws = Real.curMantissa
		Real.curMantissa += 1
		nn = abs(n)
		if nn == 0 {
			copy(Real.One, b:&b)									/* x^0 = 1 */
			Real.curMantissa = nws; return
		} else if nn == 1 { copy(a, b: &b)					/* x^1 = x */
		} else if nn == 2 { Mul(&t.real, a:a, b:a); copy(t.real, b: &b)    /* x^2 = x*x */
		} else {
			/* apply Knuth's algorithm */
			copy(Real.One, b:&b);	/* b = 1 */
			copy(a, b:&r.real);	/* r = a */
			while true {
				if ODD(nn) { Mul(&t.real, a:b, b:r.real); copy(t.real, b:&b) }
				nn = nn / 2; if nn == 0 { break }
				Mul(&r.real, a:r.real, b:r.real)
			}
		}
		
		/* take reciprocal if n<0 */
		if n<0 { Div(&t.real, a:Real.One, b:b); copy(t.real, b:&b) }
		
		/* restore original precision */
		Real.curMantissa = nws; Round(&b)
	} //IntPower;

	private static func Cmp (_ a: RealArray, b: RealArray) -> Int {
		/*
		This routine compares the extended numbers `a' and `b' and
		returns the value -1, 0, or 1 dep}ing on whether `a'<`b',
		`a'=`b', or `a'>`b'.  It is faster than merely subtracting
		`a' and `b' and looking at the sign of the result.
		*/
		var ia, ib, ma, mb, na, nb: Int
		
		ia = Sign(1, y:a[0]); if a[0] == 0 { ia = 0 }
		ib = Sign(1, y:b[0]); if b[0] == 0 { ib = 0 }
		
		/* compare signs */
		if ia != ib { return Sign(1, y:Double(ia-ib)) }
		
		/* signs are the same, compare exponents */
		ma = Int(a[1]); mb = Int(b[1]);
		if ma != mb { return ia * Sign(1, y:Double(ma-mb)) }
		
		/* signs & exponents are the same, compare mantissas */
		na = Min(Int(abs(a[0])), y:Real.curMantissa); nb = Min(Int(abs(b[0])), y:Real.curMantissa);
		for i in 2...Min(na, y:nb)+1 {
			if a[i] != b[i] { return ia*Sign(1, y:a[i]-b[i]) }
		};
		
		/* mantissas are the same to the common length, compare lengths */
		if na != nb { return ia * Sign(1, y:Double(na-nb)) }
		
		/* signs, exponents, mantissas, and legnths are the same so a=b */
		return 0
	} //Cmp;

	private static func Sqrt (_ b: inout RealArray, a: RealArray) {
		/*
		Computes the square root of `a' and returns the result
		in `b'.
		
		This routine employs the following Newton-Raphson iteration, which
		converges to 1 / Sqrt(a):
		
		X(k+1) = X(k) + 0.5 * (1 - X(k)^2 * a) * X(k)
		
		where the multiplication () * X(k) is performed with only half the
		normal level of precision.  These iterations are performed with a
		maximum precision level curMantissa that is dynamically changed,
		doubling with each iteration.  The final iteration is performed
		as follows (this is due to A. Karp):
		
		Sqrt(a) = (a * X(n)) + 0.5 * (a - (a * X(n)^2) * X(n)
		
		where the multiplications a * X(n) and ()* X(n) are performed
		with only half of the final level of precision.
		*/
		var t1, t2: Double
		var k0 = FixedLReal() //Real(size: Real.curMantissa+4)
		var k1 = FixedLReal() //Real(size: Real.curMantissa+4)
		var k2 = FixedLReal() //Real(size: Real.curMantissa+4)
		var iq: Bool
		var ia, na, nws, n2, n, nw1, nw2, mq: Int
		
		if Real.err != 0 { Zero(&b); return }
		ia = Sign(1, y:a[0]); na = Min(Int(abs(a[0])), y:Real.curMantissa)
		
		/* trivial values */
		if na == 0 { Zero(&b); return }
		
		/* error checking */
		if ia < 0 {
			print("*** Sqrt: Argument is negative!")
			Real.err = 70; return
		}
		nws = Real.curMantissa
		
		/* determine the least integer mq such that 2^mq >= curMantissa */
		t1 = Double(Real.curMantissa); mq = Int(Real.invLn2*log(t1)+1-Real.mprxx)
		
		/* initial approximation of 1 / Sqrt(a) */
		n = 0; t1 = 0
		RealToNumbExp(a, b: &t1, n: &n)
		n2 = -(n / 2); t2 = sqrt(t1*ipower(2.0, base: n+2*n2))
		t1 = 1/t2
		NumbExpToReal(t1, n:n2, b:&b)
		Real.curMantissa = 3; iq = false
		
		/* perform the Newton_Raphson iteration described above with
		a dynamically changing precision level curMantissa (one greater
		than the powers of two). */
        for k in 2...mq-1 {
			nw1 = Real.curMantissa; Real.curMantissa = Min(2*Real.curMantissa-2, y:nws)+1
			nw2 = Real.curMantissa
			while true {
				Mul(&k0.real, a: b, b: b)           /* k0 = X(k)^2 */
				Mul(&k1.real, a: a, b: k0.real)         /* k1 = a * X(k)^2 */
				Sub(&k0.real, a:Real.One, b:k1.real)       /* k0 = 1 - a * X(k)^2 */
				Real.curMantissa = nw1
				Mul(&k1.real, a:b, b:k0.real)          /* k1 = X(k)*(1 - a * X(k)^2) */
				Muld(&k0.real, a: k1.real, b: Real.HALF, n: 0)   /* k0 = 0.5 * (X(k)*(1 - a * X(k)^2)) */
				Real.curMantissa = nw2
				Add(&b, a:b, b:k0.real)           /* X(k+1) = X(k) + 0.5 * (X(k)*(1 - a * X(k)^2)) */
				if !iq && (k == mq-Real.NIT) {
					iq = true
				} else {
					break
				}
			}
		}
		
		/* last iteration using Karp's trick */
		Mul(&k0.real, a:a, b:b);              /* k0 = a * X(n) */
		nw1 = Real.curMantissa
		Real.curMantissa = Min(2*Real.curMantissa-2, y:nws)+1
		nw2 = Real.curMantissa
		Mul(&k1.real, a:k0.real, b:k0.real)            /* k1 = (a * X(n))^2 */
		Sub(&k2.real, a:a, b:k1.real)             /* k2 = a - (a * X(n))^2 */
		Real.curMantissa = nw1
		Mul(&k1.real, a:k2.real, b:b)              /* k1 = X(n) * (a - (a * X(n))^2) */
		Muld(&k2.real, a:k1.real, b:Real.HALF, n:0)       /* k2 = 0.5 * (X(n) * (a - (a * X(n))^2)) */
		Real.curMantissa = nw2
		Add(&k1.real, a:k0.real, b:k2.real)             /* Sqrt(a) = a * X(n) + 0.5 * (X(n) * (a - (a * X(n))^2)) */
		copy(k1.real, b:&b)
		
		/* restore original resolution */
		Real.curMantissa = nws; Round(&b)
	} //Sqrt;

    /*
    Computes the n'th root of `a' and returns the result in `b'.
    `n' must be at least one and must not exceed 2^30.
    
    This routine employs the following Newton-Raphson iteration, which
    converges to a ^ (-1/n):
    
    X(k+1) = X(k) + (X(k)/n) * (1 - X(k)^n * a)
    
    The reciprocal of the final approximation to a ^ (-1/n) is the
    nth root.  These iterations are performed with a maximum precision
    level curMantissa that is dynamically changed, approximately doubling
    with each iteration.
    
    When n is large and a is very near one, the following binomial
    series is employed instead of the Newton scheme:
    
    (1+x)^(1/n) = 1 + x/n + x^2*(1-n)/(2!*n^2) + ...
    */
	private static func Root (_ b: inout RealArray, a: RealArray, n: Int) {
		let maxN = 0x40000000   /* 2^30 */
		var t1, t2, tn: Double
		var k0 = Real(size: Real.curMantissa+4)
		var k1 = Real(size: Real.curMantissa+4)
		var k2 = Real(size: Real.curMantissa+4)
		var k3 = Real(size: Real.curMantissa+4)
        var f2 = RealArray(repeating:0, count:8)
		var iq: Bool
		var nws: Int
		var ia, na, n2, k, mq, n1, n3: Int
        var a = a
		
		if Real.err != 0 { Zero(&b); return };
		ia = Sign(1, y:a[0]); na = Min(Int(abs(a[0])), y:Real.curMantissa)
		
		/* trivial values */
		if na == 0 { Zero(&b); return }
		
		/* error checking */
		if ia < 0 {
			if ODD(n) {
				a[0] = -a[0] /* continue with abs(a) */
			} else {
				print("*** Root: Argument is negative!");
				Real.err = 70; return
			}
		};
		if (n <= 0) || (n > maxN) {
			print("*** Root: Improper value of n!")
			Real.err = 60; return
		}
		nws = Real.curMantissa;
		
		/* if n = 1 or 2 use faster local routines */
		if n == 1 {
			copy(a, b:&b)
			b[0] = Double(Sign(Int(b[0]), y: Double(ia)))
			return
		} else if n == 2 {
			Sqrt(&b, a:a); return
		}
		
		/* determine the least integer mq such that 2^mq >= curMantissa */
		t1 = Double(Real.curMantissa); mq = Int(Real.invLn2*log(t1)+1-Real.mprxx)
		
		/* check how close `a' is to 1 */
		Sub(&k0.real, a:a, b:Real.One)
		if k0.real[0] == 0 { copy(Real.One, b:&b); return }
		n1 = 0; t1 = 0
		RealToNumbExp(k0.real, b: &t1, n: &n1)
		n2 = Int(Real.invLn2*log(abs(t1)))
		t1 = t1*ipower(Real.HALF, base: n2)
		n1 += n2
		if n1 <= -30 {
			t2 = Double(n); n2 = Int(Real.invLn2*log(t2)+1+Real.mprxx);
			n3 = -Real.NBT*Real.curMantissa / n1
			if n3 < Int(1.25*Double(n2)) {
				/* `a' is so close to 1 that it is cheaper to use the
				binomial series */
				Real.curMantissa += 1
				Divd(&k1.real, a: k0.real, b: t2, n: 0); Add(&k2.real, a:Real.One, b:k1.real)
				k = 0
				while true {
					k += 1; t1 = 1-Double(k*n); t2 = Double((k+1)*n)
					Muld(&k2.real, a: k1.real, b: t1, n: 0)
					Muld(&k1.real, a: k3.real, b: t2, n: 0)
					Mul(&k3.real, a:k0.real, b:k1.real)
					copy(k3.real, b:&k1.real)
					Add(&k3.real, a:k1.real, b:k2.real)
					copy(k3.real, b:&k2.real)
					if (k1.real[0] == 0) || (k1.real[1] < Double(-Real.curMantissa)) {
						break
					}
				}
				copy(k2.real, b:&b); Div(&k0.real, a:Real.One, b:k2.real)
				Real.curMantissa = nws; Round(&b)
				b[0] = Double(Sign(Int(b[0]), y: Double(ia)))
				return
			}
		}
		
		/* compute the initial approximation of a^(-1/n) */
		tn = Double(n); RealToNumbExp(a, b: &t1, n: &n1)
		n2 = -Int(Double(n1)/tn)
		t2 = exp(-1/tn * (log(Double(t1)) + Double(n1 + Int(tn) * n2) * Real.Ln2))
		NumbExpToReal(t2, n: n2, b: &b)
		NumbExpToReal(tn, n: 0, b: &f2)
		Real.curMantissa = 3; iq = false
		
		/* perform the Newton_Raphson iteration described above
		with a dynamically changing precision level curMantissa
		which is one greater than the powers of two. */
        for k in 2...mq {
			Real.curMantissa = Min(2*Real.curMantissa-2, y: nws)+1
			while true {
				IntPower(&k0.real, a:b, n:n)
				Mul(&k1.real, a:a, b:k0.real)
				Sub(&k0.real, a:Real.One, b:k1.real)
				Mul(&k1.real, a:b, b:k0.real)
				Divd(&k0.real, a: k1.real, b: tn, n: 0)
				Add(&k1.real, a:b, b:k0.real)
				copy(k1.real, b:&b)
				if !iq && (k == mq-Real.NIT) {
					iq = true
				} else {
					break
				}
			}
		}
		
		/* take reciprocal to give final result */
		Div(&k1.real, a: Real.One, b: b); copy(k1.real, b: &b)
		
		/* restore original resolution */
		Real.curMantissa = nws; Round(&b)
		b[0] = Double(Sign(Int(b[0]), y: Double(ia)))
	} //Root;

	private static func Pi (_ pi: inout RealArray) {
		/*
		Computes Pi to available precision (curMantissa words).
		
		The algorithm that is used, which is due to Salamin and
		Brent, is as follows:
		
		Set A(0) = 1, B(0) = 1/Sqrt(2), D(0) = Sqrt(2) - 1/2.
		
		{ from k = 1 iterate the following operations:
		
		A(k) = 0.5 * (A(k-1) + B(k-1))
		B(k) = Sqrt (A(k-1) * B(k-1))
		D(k) = D(k-1) - 2^k * (A(k) - B(k))^2
		
		{ P(k) = (A(k) + B(k))^2 / D(k) converges quadratically to
		Pi.  In other words, each iteration approximately doubles the
		number of correct digits, providing all iterations are done
		with the maximum precision.
		*/
		var f = RealArray(repeating:0, count:3)
		var An = FixedLReal() // Real(size: Real.curMantissa+4)
		var t = FixedLReal() // Real(size: Real.curMantissa+4)
		var Bn = FixedLReal() // Real(size: Real.curMantissa+4)
		var Dn = FixedLReal() // Real(size: Real.curMantissa+4)
		var r = FixedLReal() // Real(size: Real.curMantissa+4)
		var nws, mq: Int
		var t1: Double
		
		if Real.err != 0 { Zero(&pi); return }
		
		/* increase working resolution */
		nws = Real.curMantissa; Real.curMantissa += 1
		
		/* determine the number of iterations required for the given
		precision level.  This formula is good only for this Pi
		algorithm. */
		t1 = Double(nws)*log10(Real.radix)
		mq = Int(Real.invLn2*(log(t1)-1)+1)
		
		/* initialize working variables */
		copy(Real.One, b:&An.real)						  /* A(0) = 1 */
		f[0] = 1; f[1] = 0; f[2] = 2
		Sqrt(&t.real, a:f)                           /* t = Sqrt(2) */
		Muld(&Bn.real, a: t.real, b: Real.HALF, n: 0);               /* B(0) = 1 / Sqrt(2) */
		f[1] = -1; f[2] = Real.HALF*Real.radix
		Sub(&Dn.real, a:t.real, b:f)                        /* D(0) = Sqrt(2) - 1/2 */
		
		/* perform iterations as above */
		for k in 1...mq {
			Mul(&t.real, a:An.real, b:Bn.real)                     /* t = A(k-1) * B(k-1) */
			Add(&r.real, a:An.real, b:Bn.real)                     /* r = A(k-1) + B(k-1) */
			Sqrt(&Bn.real, a: t.real)                        /* B(k) = Sqrt(A(k-1) * B(k-1)) */
			Muld(&An.real, a: r.real, b: Real.HALF, n: 0)               /* A(k) = 0.5 * (A(k-1) + B(k-1)) */
			Sub(&t.real, a:An.real, b:Bn.real)                     /* t = A(k) - B(k) */
			Mul(&t.real, a:t.real, b:t.real)                       /* t = (A(k) - B(k))^2 */
			t1 = ipower(2.0, base: k)              /* t1 = 2^k */
			Muld(&t.real, a: t.real, b: t1, n: 0)                  /* t = 2^k * (A(k) - B(k))^2 */
			Sub(&Dn.real, a:Dn.real, b:t.real)                     /* D(k) = D(k-1) -  2^k * (A(k) - B(k))^2 */
		}
		
		/* complete the computation */
		Add(&t.real, a:An.real, b:Bn.real)                       /* t = A(k) + B(k) */
		Mul(&t.real, a:t.real, b:t.real)                         /* t = (A(k) + B(k))^2 */
		Div(&pi, a:t.real, b:Dn.real)                       /* k2 = (A(k) + B(k))^2 / D(k) */
		
		/* back to original precision */
		Real.curMantissa = nws; Round(&pi)
	} //Pi;

	private static func Entier (_ b: inout RealArray, a: RealArray) {
		/*
		Set `b' to the largest integer not greater than `a'.
		For example: Int(3.6) = 3 and Int(-1.6)=-2
		*/
		var ia, na, ma, nb: Int
		
		if Real.err != 0 { Zero(&b); return }
		ia = Sign(1, y: a[0]); na = Min(Int(abs(a[0])), y: Real.curMantissa)
		ma = Int(a[1])
		
		/* check for zero -> result is zero */
		if na == 0 { Zero(&b); return }
		
		/* check if `a' can be represented exactly as an integer */
		if ma >= Real.curMantissa {
			print("*** Entier: Argument is too large!")
		}
		
		/* place integer part of a in b */
		nb = Min(Max(ma+1, y:0), y:na)
		if nb == 0 {
			Zero(&b); return
		} else {
			b[0] = Double(Sign(nb, y: Double(ia))); b[1] = Double(ma)
			b[nb+2] = 0; b[nb+3] = 0
			for i in 2...nb+1 { b[i] = a[i] }
		}
		
		/* if (a < 0) & (Frac(a) != 0) { b = b - 1 */
		if ia == -1 {
			for i in nb+2...na+1 {
				if a[i] != 0 { Sub(&b, a:b, b:Real.One); return }
			}
		}
	} //Entier;

	private static func RoundInt (_ b: inout RealArray, a: RealArray) {
		/*
		Set `b' to the integer nearest to the multiple precision
		number `a'.
		*/
		var ia, na, ma, ic, nc, mc, nb: Int
		var f = RealArray(repeating:0, count:3)
		var k0 = Real(size: Real.curMantissa+4)
		
		if Real.err != 0 { Zero(&b); return }
		ia = Sign(1, y:a[0]); na = Min(Int(abs(a[0])), y:Real.curMantissa)
		ma = Int(a[1])
		
		/* check for zero -> result is zero */
		if na == 0 { Zero(&b); return }
		
		/* check if `a' can be represented exactly as an integer */
		if ma >= Real.curMantissa {
			print("*** RoundInt: Argument is too large!")
			Real.err = 56; return
		}
		
		/* add or subtract 1/2 from the input, dep}ing on its sign */
		f[0] = 1; f[1] = -1; f[2] = Real.HALF*Real.radix
		if ia == 1 { Add(&k0.real, a:a, b:f) } else { Sub(&k0.real, a:a, b:f) }
		ic = Sign(1, y:k0.real[0]); nc = Int(abs(k0.real[0])); mc = Int(k0.real[1])
		
		/* place integer part of k0 in b */
		nb = Min(Max(mc+1, y:0), y:nc)
		if nb == 0 {
			Zero(&b)
		} else {
			b[0] = Double(Sign(nb, y:Double(ic))); b[1] = Double(mc)
			b[nb+2] = 0; b[nb+3] = 0
			for i in 2...nb+1 { b[i] = k0.real[i] }
		}
	} //RoundInt;

	private static func Exp (_ b: inout RealArray, a: RealArray) {
		/*
		This computes the exponential function of the extended
		precision number `a' and returns the result in `b'.  The
		value of `ln2' is also required.
		
		This algorithm uses a modification of the Taylor's series
		for Exp(t):
		
		Exp(t) = (1 + r + r^2/2! + r^3/3! + r^4/4! ...) ^ q*2^n
		
		where q = 256, r = t'/q, t' = t - n Ln(2) and where n is
		chosen so that -0.5 Ln(2) < t' <= 0.5 Ln(2).  Reducing t
		mod Ln(2) and dividing by 256 ensures that -0.001<r<=0.001
		which accelerates convergence in the above series.
		*/
		let NQ = 8
		var t1, t2, tl: Double
		var nws, n1, nz, l1: Int
		var k0 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k1 = FixedLReal() // Real(size: Real.curMantissa+4)FixedLReal() //
		var k2 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k3 = FixedLReal() // Real(size: Real.curMantissa+4)
		
		if Real.err != 0 { Zero(&b); return };
		// let na = Min(Int(abs(a[0])), y:Real.curMantissa)
		t1 = 0; n1 = 0
		RealToNumbExp(a, b: &t1, n: &n1)
		t1 = t1*ipower(2, base: n1)
		Zero(&k1.real)
		
		/* unless the argument is near Ln(2), ln2 must be precomputed.
		This exception is necessary because Ln calls Exp to
		initialize ln2 */
		if (abs(t1-Real.Ln2) > Real.invRadix) && (Real.Log2.count == 0) {
			Real.Initialize()
//			print("*** Exp: ln2 must be precomputed!")
//			Real.err = 34; return
		}
		
		/* check for overflows and underflows */
		if t1 >= 1.0E9 {
			if t1 > 0 {
				print("*** Exp: Argument is too large \(t1) x 10^\(n1)")
				Real.err = 35
			} else {
				Zero(&b)
			}
			return
		}
		
		/* increase resolution */
		nws = Real.curMantissa; Real.curMantissa += 1
		
		/* compute the reduced argument a' = a - Ln(2) * Int(a/Ln(2)).
		Save nz = Int(a/Ln(2)) for correcting the exponent of the
		final result. */
		if abs(t1-Real.Ln2) > Real.invRadix {
			Div(&k0.real, a:a, b:Real.Log2)
			RoundInt(&k1.real, a: k0.real)
			RealToNumbExp(k1.real, b: &t1, n: &n1)
			nz = Int(t1*Double(ipower(2, base: n1)) + Double(Sign(Int(Real.mprxx), y: t1)))
			Mul(&k2.real, a:Real.Log2, b:k1.real)
			Sub(&k0.real, a:a, b:k2.real)
		} else {
			copy(a, b:&k0.real); nz = 0
		}
		tl = k0.real[1] - Double(Real.curMantissa)
		
		/* check if the reduced argument is zero */
		if k0.real[0] == 0 {
			copy(Real.One, b:&k0.real)
		} else {
			/* divide the reduced argument by 2^nq */
			Divd(&k1.real, a: k0.real, b: 1, n: NQ)
			
			/* compute Exp using the usual Taylor series */
			copy(Real.One, b:&k2.real); copy(Real.One, b:&k3.real); l1 = 0
			while true {
				l1 += 1
				if l1 == 10000 {
					print("*** Exp: Iteration limit exceeded!")
					Real.err = 36; Real.curMantissa = nws; return
				}
				t2 = Double(l1)
				Mul(&k0.real, a:k2.real, b:k1.real)
				Divd(&k2.real, a: k0.real, b: t2, n: 0)
				Add(&k0.real, a:k3.real, b:k2.real)
				copy(k0.real, b:&k3.real)
				
				/* check for convergence of the series */
				if (k2.real[0] == 0) || (k2.real[1] < tl) { break }
			}
			
			/* raise to the (2^nq)-th power */
			for _ in 1...NQ { Mul(&k0.real, a:k0.real, b:k0.real) }
		}
		
		/* multiply by 2^nz */
		Muld(&k1.real, a:k0.real, b:1, n:nz)
		copy(k1.real, b:&b)
		
		/* restore original precision level */
		Real.curMantissa = nws; Round(&b)
	} //Exp;

	private static func Ln (_ b: inout RealArray, a: RealArray) {
		/*
		This routine computes the natural logarithm of the extended
		precision number `a' and returns the extended precision
		result in `b'.  This routine uses a previously calculated
		value `ln2'.  if `a' is close to 2 then `ln2' is not required
		so this func can be used to compute `ln2'.
		
		The Taylor series for Ln converges much more slowly than that
		of Exp.  Thus this routine does not employ the Taylor series,
		but instead computes logarithms by solving Exp(b) = a using the
		following Newton iteration, which converges to `b':
		
		X(k+1) = X(k) + (a - Exp(X(k)) / Exp(X(k))
		
		These iterations are performed with a maximum precision level
		Real.curMantissa that is dynamically changed, approximately doubling
		with each iteration.
		*/
		var ia, na, n1, nws, mq: Int
		var t1, t2: Double
		var k0 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k1 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k2 = FixedLReal() // Real(size: Real.curMantissa+4)
		var iq: Bool
		
		if Real.err != 0 { Zero(&b); return }
		
		/* check for error inputs */
		ia = Sign(1, y:a[0]); na = Min(Int(abs(a[0])), y:Real.curMantissa)
		if (ia < 0) || (na == 0) {
			print("*** Ln: Argument is less than or equal to zero!")
			Real.err = 50; return
		}
		
		/* unless the input is close to 2, ln2 must be known */
		t1 = 0; n1 = 0
		RealToNumbExp(a, b: &t1, n: &n1)
		if ((abs(t1-2.0) > 1.0e-3) || (n1 != 0)) && (Real.Log2.count == 0) {
			Real.Initialize()
//			print("*** Ln: Ln(2) must be precomputed!"); Real.err = 51; return
		}
		
		/* check if input is exactly one */
		if (a[0] == 1) && (a[1] == 0) && (a[2] == 1) { Zero(&b); return }
		
		/* determine the least integer mq such that 2^mq >= curMantissa */
		nws = Real.curMantissa; t2 = Double(nws); mq = Int(Real.invLn2*log(t2)+1-Real.mprxx)
		
		/* compute initial approximation of Ln(a) */
		t1 = log(t1)+Double(n1)*Real.Ln2; NumbExpToReal(t1, n: 0, b: &b)
		Real.curMantissa = 3; iq = false
		for k in 2...mq {
			Real.curMantissa = Min(2*Real.curMantissa-2, y:nws)+1
			while true {
				Exp(&k0.real, a:b)		   /* k0 = Exp(X(k)) */
				Sub(&k1.real, a:a, b:k0.real)    /* k1 = a - Exp(X(k)) */
				Div(&k2.real, a:k1.real, b:k0.real)   /* k2 = (a - Exp(X(k))) / Exp(X(k)) */
				Add(&k1.real, a:b, b:k2.real)    /* k1 = X(k) + (a - Exp(X(k))) / Exp(X(k)) */
				copy(k1.real, b:&b)
				if (k == mq-Real.NIT) && !iq {
					iq = true
				} else {
					break
				}
			}
		};
		
		/* restore original precision */
		Real.curMantissa = nws; Round(&b)
	} //Ln;

    /**
    This routine computes the cosine and sine of the extended
    precision number *a* and returns the results in *cos* and
    *sin*, respectively.  The units of *a* are in radians.
    
    The calculations are performed using the conventional Taylor's
    series for Sin(s):
    
    Sin(s) = s - s³/3! + s⁵/5! - s⁷/7! ....
    
    where s = t - a \* π/2 - b \* π/16 and the integers a and b are
    chosen to minimize the absolute value of s.  We can then compute
    
    Sin(t) = Sin(s + a \* π/2 + b \* π/16),
    Cos(t) = Cos(s + a \* π/2 + b \* π/16)
    
    by applying elementary trig identities for sums.  The sine and
    cosine of b \* π/16 are of the form 1/2 \* Sqrt(2 ± Sqrt(2 ± Sqrt(2))).
    Reducing t in this manner ensures that -π/32 < s < π/32 which
    accelerates convergence in the above series.
    */
	private static func SinCos (_ sin: inout RealArray, cos: inout RealArray, a: RealArray) {

		var t1, t2: Double
		var nws: Int
		var f = RealArray(repeating:0, count:3)
		var na, ka, kb, n1, kc, l1: Int
		var k0 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k1 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k2 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k3 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k4 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k5 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k6 = FixedLReal() // Real(size: Real.curMantissa+4)
		
		if Real.err != 0 { Zero(&sin); Zero(&cos); return }
		
		// print("Sincos angle ="); Write(a)
		
		na = Min(Int(abs(a[0])), y:Real.curMantissa)
		
		/* check for trivial case when a = 0 */
		if na == 0 { copy(Real.One, b:&cos); Zero(&sin); return }
		
		/* check if pi has been precomputed */
		if Real.Pi.count == 0 {
			Real.Initialize()
//			print("*** SinCos: pi must be precomputed!")
//			Real.err = 28; return
		}
		
		/* increase resolution */
		nws = Real.curMantissa; Real.curMantissa += 1
		
		/* reduce input to between -pi and pi */
		Muld(&k0.real, a: Real.Pi, b: 2.0, n: 0)
		Div(&k1.real, a:a, b:k0.real)
		RoundInt(&k2.real, a:k1.real)       /* k2 = a DIV 2pi */
		Sub(&k3.real, a:k1.real, b:k2.real)    /* k3 = a MOD 2pi */
		
		/* determine the nearest multiple of pi/2, and within a
		quadrant, the nearest multiple of π/16.  Through most
		of the rest of this procedure, ka and kb are the integers
		a and b of the above algorithm. */
		t1 = 0; n1 = 0
		RealToNumbExp(k3.real, b:&t1, n:&n1)
		if n1 >= -Real.NBT {
			t1 *= ipower(2, base: n1); t2 = 4*t1
			ka = lround(t2)  // t2 < 0 ? -Int(Real.HALF-t2) : Int(t2+Real.HALF)   /* ka = rm.round(t2) */
			t1 = 8 * (t2 - Double(ka))
			kb = lround(t1)   // t1 < 0 ? -Int(Real.HALF-t1) : Int(t1+Real.HALF)   /* kb = rm.round(8*(t2-ka)) */
			/* ka = Int(t2); kb = rm.round(8*(t2-ka)) */
		} else {
			ka = 0; kb = 0
		}
        t1 = Double(8*ka+kb)/32.0
		NumbExpToReal(t1, n: 0, b: &k1.real)
		Sub(&k2.real, a:k3.real, b:k1.real)
		Mul(&k1.real, a:k0.real, b:k2.real)
		
		/* compute cosine and sine of the reduced arguments using
		Taylor's series */
		if k1.real[0] == 0 {
			Zero(&k0.real)
		} else {
			copy(k1.real, b:&k0.real); Mul(&k2.real, a:k0.real, b:k0.real); l1 = 0
			while true {
				l1 += 1
				if l1 == 10000 {
					print("*** SinCos: Iteration limit exceeded!")
					Real.err = 29; Real.curMantissa = nws; return
				}
                t2 = -(2.0*Double(l1))*(2.0*Double(l1)+1.0)
				Mul(&k3.real, a:k2.real, b:k1.real)
				Divd(&k1.real, a: k3.real, b: t2, n: 0)
				Add(&k3.real, a:k1.real, b:k0.real)
				copy(k3.real, b:&k0.real)
				
				/* check for convergence of the series */
				if (k1.real[0] == 0) || (k1.real[1] < (k0.real[1]-Double(Real.curMantissa))) { break }
			}
		}
		
		/* compute Cos(s) = Sqrt(1-Sin(s)^2) */
		copy(k0.real, b:&k1.real)
		Mul(&k2.real, a:k0.real, b:k0.real); Sub(&k3.real, a:Real.One, b:k2.real); Sqrt(&k0.real, a:k3.real)
		
		/* compute cosine and sine of b*Pi/16 */
		kc = abs(kb); f[0] = 1; f[1] = 0; f[2] = 2.0
		if kc == 0 {
			copy(Real.One, b:&k2.real); Zero(&k3.real)
		} else {
			switch kc {
			case 1: Sqrt(&k4.real, a:f); Add(&k5.real, a:f, b:k4.real); Sqrt(&k4.real, a:k5.real)
			case 2: Sqrt(&k4.real, a:f)
			case 3: Sqrt(&k4.real, a:f); Sub(&k5.real, a:f, b:k4.real); Sqrt(&k4.real, a:k5.real)
			case 4: Zero(&k4.real)
			default: break /* do nothing */
			}
			Add(&k5.real, a:f, b:k4.real); Sqrt(&k3.real, a:k5.real); Muld(&k2.real, a: k3.real, b: Real.HALF, n: 0)
			Sub(&k5.real, a:f, b:k4.real); Sqrt(&k4.real, a:k5.real); Muld(&k3.real, a: k4.real, b: Real.HALF, n: 0)
		}
		
		/* apply the trigonometric summation identities to compute
		cosine and sine of s + b*Pi/16 */
		if kb < 0 { k3.real[0] = -k3.real[0] }
		Mul(&k4.real, a:k0.real, b:k2.real); Mul(&k5.real, a:k1.real, b:k3.real); Sub(&k6.real, a:k4.real, b:k5.real)
		Mul(&k4.real, a:k1.real, b:k2.real); Mul(&k5.real, a:k0.real, b:k3.real); Add(&k1.real, a:k4.real, b:k5.real)
		copy(k6.real, b:&k0.real)
		
		/* this code applies the trig summation identities for (s + b*π/16 + a*π/2 */
		switch ka {
		case 0: copy(k0.real, b:&cos); copy(k1.real, b:&sin)
		case 1: copy(k1.real, b:&cos); cos[0] = -cos[0]; copy(k0.real, b:&sin)
		case -1: copy(k1.real, b:&cos); copy(k0.real, b:&sin); sin[0] = -sin[0]
		case 2,
		    -2: copy(k0.real, b:&cos); cos[0] = -cos[0]; copy(k1.real, b:&sin); sin[0] = -sin[0]
		default: break /* do nothing */
		}
		
		/* restore the orginal precision level */
		Real.curMantissa = nws; Round(&cos); Round(&sin)
	} //SinCos;

	private static func SinhCosh (_ sinh: inout RealArray, cosh: inout RealArray, a: RealArray) {
		/*
		This routine computes the hyperbolic cosine and sine of the
		number `a' and returns the two results in `cosh' and `sinh',
		respectively.  The last word of the result is not reliable.
		*/
		var k0 = Real(size: Real.curMantissa+4)
		var k1 = Real(size: Real.curMantissa+4)
		var k2 = Real(size: Real.curMantissa+4)
		var nws: Int
		
		nws = Real.curMantissa; Real.curMantissa += 1
		Exp(&k0.real, a: a); Div(&k1.real, a:Real.One, b:k0.real)			/* k1 = exp(-a); k0 = exp(a) */
		Add(&k2.real, a:k0.real, b:k1.real)										/* k2 = exp(a) + exp(-a) */
		Muld(&k2.real, a: k2.real, b: Real.HALF, n: 0); copy(k2.real, b: &cosh)	/* cosh = (exp(a) + exp(-a))/2 */
		Sub(&k2.real, a:k0.real, b:k1.real)										/* k2 = exp(a) - exp(-a) */
		Muld(&k2.real, a: k2.real, b: Real.HALF, n: 0); copy(k2.real, b: &sinh)	/* sinh = (exp(a) - exp(-a))/2 */
		
		/* restore orginal precision */
		Real.curMantissa = nws; Round(&cosh); Round(&sinh)
	} //SinhCosh;

	private static func ATan2 (_ a: inout RealArray, x:RealArray, y: RealArray) {
		/*
		This routine computes the angle `a' subt}ed by the
		pair (`x', `y') considered as a point in the x-y plane.
		This routine places the resultant angle correctly in the
		full circle where -π < `a' <= π.  The last word of the
		result is not reliable.
		
		The Taylor series for Arcsin converges much more slowly than
		that of Sin.  Thus this routine does not employ a Taylor
		series, but instead computes Arccos or Arcsin by solving
		Cos(a) = x or Sin(a) = y using one of the following Newton
		iterations, both of which converge to `a':
		
		z(k+1) = z(k) - (x - Cos(z(k))) / Sin(z(k))
		z(k+1) = z(k) - (y - Sin(z(k))) / Cos(z(k))
		
		The first is selected if abs(x) <= abs(y); otherwise the
		second is used.  These iterations are performed with a
		maximum precision level curMantissa that is dynamically changed,
		approximately doubling with each iteration.
		*/
		let NIT=3
		var t1, t2, t3: Double
		var iq: Bool
		var nws: Int
		var ix, iy, nx, ny, mq, n1, n2, kk: Int
		var k0 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k1 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k2 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k3 = FixedLReal() // Real(size: Real.curMantissa+4)
		var k4 = FixedLReal() // Real(size: Real.curMantissa+4)
		
		if Real.err != 0 { Zero(&a); return }
		ix = Sign(1, y:x[0]); nx = Min(Int(abs(x[0])), y:Real.curMantissa)
		iy = Sign(1, y:y[0]); ny = Min(Int(abs(y[0])), y:Real.curMantissa)
		
		/* check if both x and y are zero */
		if (nx == 0) && (ny == 0) {
			print("*** ATan2: Both arguments are zero!")
			Real.err = 7; return
		}
		
		/* check if π has been precomputed */
		if Real.Pi.count == 0 {
			Real.Initialize()
//			print("*** ATan2: Pi must be precomputed!")
//			Real.err = 8; return
		}
		
		/* check if one of x or y is zero */
		if nx == 0 {
			if iy > 0 {
				Muld(&a, a: Real.Pi, b: Real.HALF, n: 0)
			} else {
				Muld(&a, a: Real.Pi, b: -Real.HALF, n: 0)
			}
			return
		} else if ny == 0 {
			if ix > 0 { Zero(&a) } else { copy(Real.Pi, b:&a) }
			return
		}
		
		/* increase the resolution */
		nws = Real.curMantissa; Real.curMantissa += 1
		
		/* determine the least integer mq such that 2^mq >= curMantissa */
		mq = Int(Real.invLn2*log(Double(nws))+1-Real.mprxx)
		
		/* normalize x and y so that x^2 + y^2 = 1 */
		Mul(&k0.real, a:x, b:x); Mul(&k1.real, a:y, b:y); Add(&k2.real, a:k0.real, b:k1.real); Sqrt(&k3.real, a:k2.real)
		Div(&k1.real, a:x, b:k3.real); Div(&k2.real, a:y, b:k3.real)
		
		/* compute initial approximation of the angle */
		n1 = 0; t1 = 0; n2 = 0; t2 = 0
		RealToNumbExp(k1.real, b: &t1, n: &n1); RealToNumbExp(k2.real, b: &t2, n: &n2)
		n1 = Max(n1, y:-66); n2 = Max(n2, y:-66)
		t1 = t1*ipower(2, base: n1); t2 = t2*ipower(2, base: n2)
		t3 = atan2(t2, t1)
		NumbExpToReal(t3, n: 0, b: &a)
		
		/* the smaller of x or y will be used from now on to measure convergence.
		This selects the Newton iteration (of the two listed above) that has
		the largest denominator */
		if abs(t1) <= abs(t2) {
			kk = 1; copy(k1.real, b:&k0.real)
		} else {
			kk = 2; copy(k2.real, b:&k0.real)
		}
		
		/* perform the Newton-Raphson iteration described above with a dynamically
		changing precision level curMantissa (one greater than powers of two). */
		Real.curMantissa = 3; iq = false
		for k in 2...mq {
			Real.curMantissa = Min(2*Real.curMantissa-2, y:nws)+1
			while true {
				SinCos(&k2.real, cos: &k1.real, a: a)
				if kk == 1 {
					Sub(&k3.real, a:k0.real, b:k1.real); Div(&k4.real, a:k3.real, b:k2.real); Sub(&k1.real, a:a, b:k4.real)
				} else {
					Sub(&k3.real, a:k0.real, b:k2.real); Div(&k4.real, a:k3.real, b:k1.real); Add(&k1.real, a:a, b:k4.real)
				}
				copy(k1.real, b:&a)
				if !iq && (k == mq-NIT) { iq = true } else { break }
			}
		}
		
		/* restore the original precision */
		Real.curMantissa = nws; Round(&a)
	} //ATan2;

	private static func fromString (_ string: String) -> Real {
		/**
		Converts the number in `str' to an extended Real and
		returns the result.  The number representation is
		given by:
	
		number = ["+"|"-"] @{digit@} ["." @{digit@}] [scale]
		
		where  scale = "E"|"D" ["+"|"-"] digit @{digit@}
		and    digit = "0".."9" | " "
		
		Thus the following is a valid input number:
		
		"1.23456 12938 23456 E + 200"
		
		Note: This real number definition is backwardly
		compatible with the Oberon-2 real string but has
		been extended to allow splitting of very large
		numbers into more readable segments by inserting
		spaces.
		*/
		var s = FixedLReal() // Real(size: Real.curMantissa+4)
		var nexp, es, is1, dig, nws, dp: Int
		var isZero = false
        var f = RealArray(repeating: 0, count:8)
		var str = string
		var b : Real
		var ch : Character
		
        func TrimBlanks (_ s : String) -> String { s.trimmingCharacters(in: .whitespacesAndNewlines) }
        func GetChar (_ s: String) -> Character { (s.isEmpty ? "\0" : s[s.startIndex]) }
        func NextChar (_ s: inout String) { s.removeFirst() }
		
		func GetDigit () -> Int {
			/* skips blanks to get a digit; returns -1 on an invalid digit */
			str = TrimBlanks(str)
			let ch = GetChar(str)
			if (ch >= "0") && (ch <= "9") {
				NextChar(&str)
				if ch > "0" { isZero = false }
				let nstr = ""+[ch]
				return Int(atoi(nstr))
			} else {
				return -1
			}
		} //GetDigit;
		
		func GetSign () -> Int {
			/* skip leading blanks */
			str = TrimBlanks(str)
			
			/* check for leading sign */
			let ch = GetChar(str)
			if ch == "+" {
				NextChar(&str); return 1
			} else if ch == "-" {
				NextChar(&str); return -1
			} else {
				return 1
			}
		} //GetSign;
		
		nws = Real.curMantissa; Real.curMantissa += 1
		
		/* check for initial sign */
		is1 = GetSign()
		
		/* clear result */
		Zero(&s.real); f[0] = 1; f[1] = 0
		
		/* scan for digits, stop on a non-digit */
		isZero = true;
		while true {
			dig = GetDigit()
			if dig<0 { break }
			if !isZero { Muld(&s.real, a: s.real, b: 10.0, n: 0) }
			if dig != 0 { f[2] = Double(dig); Add(&s.real, a:f, b:s.real) }
		}
		
		/* check for decimal point */
		dp = 0; ch = GetChar(str)
		if ch == "." {
			NextChar(&str)
			while true {
				dig = GetDigit()
				if dig < 0 { break }
				Muld(&s.real, a: s.real, b: 10.0, n: 0)
				if dig != 0 {
					f[0] = 1; f[2] = Double(dig)
				} else {
					f[0] = 0
				}
				dp += 1; Add(&s.real, a:f, b:s.real)
			}
		}
		
		/* check for exponent */
		nexp = 0
		ch = GetChar(str)
		if (ch == "E") || (ch == "D") {
			NextChar(&str)
			es = GetSign()
			while true {
				dig = GetDigit()
				if dig<0 { break }
				nexp = nexp*10+dig
			}
			nexp = es*nexp  /* add the sign */
		}
		
		/* scale the resultant number */
		s.real[0] = s.real[0]*Double(is1); nexp -= dp
		f[0] = 1; f[2] = 10; b = Real(size: Real.curMantissa+4)
		IntPower(&b.real, a: f, n: nexp); Mul(&s.real, a:b.real, b:s.real); copy(s.real, b:&b.real)
		
		/* back to original resolution */
		Real.curMantissa = nws; Round(&b.real)
		return b
	} //ToReal;


	/*---------------------------------------------------------*/
	/* Conversion routines                                     */
	
	func toString (_ Decimal : Int, ExpWidth : Int, EngFormat: Bool) -> String {
		/** return the extended real number as a string with 'Decimal' decimal places and
		an exponent with 'ExpWidth' places.  'ExpWidth' <> 0 produces a scientific
		formatted number or, if 'EngFormat' is true, an engineering formatted
		number. 'ExpWidth' = 0 produces a floating-point number string. A fixed-point
		number is output when 'Decimal' > 0. */
		let log2 = 0.301029995663981195
		var InCnt, Aexp, MaxExpWidth : Int
		var nx = 0
		var nws, na, ia, nl: Int
		var aa, t1: Double
		var FixPoint = false
		var f = RealArray(repeating:0, count:8)
		var k = FixedLReal() // Real(size: Real.curMantissa+4)
		var Str = ""
		let a = self
        var ExpWidth = ExpWidth
        var Decimal = Decimal
		
		func ConcatChar (_ s: inout String, _ ch : Character) { s.append(ch) }
		func AddDigit (_ s: inout String, _ d: Int) { s += String(d) }
        func AddInt (_ s: inout String, _ n: Int) { s += String(abs(n)) }
		
		func GetDigit() {
			var nn: Int
			if k.real[0] == 0 { AddDigit(&Str, 0); return }
			if k.real[1] == 0 {
				nn = Int(k.real[2]); f[0] = 1; f[2] = Double(nn)
			} else {
				f[0] = 0; nn = 0
			}
            AddDigit(&Str, nn); Real.Sub(&k.real, a:k.real, b:f)
            Real.Muld(&k.real, a: k.real, b: 10.0, n: 0)
		} //GetDigit;
		
		func Round() {
            /* BCD-based rounding algorithm */
			var res = ""
			var c = 5
            for ch in Str.reversed() {
				if ch != "." && ch != "-" {
                    c += ch.wholeNumberValue!
					let digit = c % 10
					res = String(digit) + res
					c = c / 10
				} else {
					res = [ch] + res
				}
			}
			Str = res
			if c > 0 {
				/* insert "1" at pos 0 and move decimal to left */
                Str.insert("1", at: Str.startIndex)
				if let range = Str.firstIndex(of: ".") {
                    Str.remove(at: range)
                    Str.insert(".", at: Str.index(range, offsetBy: -1))
				}
				nx += 1
			}
            Str.removeLast()  // ignore the rounding digit
		} //Round;
		
		func Trim() {
			/* check if trailing zeros should be trimmed */
			let zero = Character("0")
            if Str.last == zero && !FixPoint {
                while Str.last == zero { Str.removeLast() }
			}
			
			/* remove trailing '.' */
            if Str.last == "." { Str.removeLast() }
		} //Trim;
		
		
		/* initialize a few parameters */
        ia = Real.Sign(1, y:a.real[0]); na = Real.Min(Int(abs(a.real[0])), y:Real.curMantissa)
        nws = Real.curMantissa; Real.curMantissa += 1; Real.Zero(&k.real)
		
		/* round the number */
		f[0] = 1; f[1] = 0; f[2] = 10; nl = 0
		
		/* determine the exact power of ten for exponent */
		if na != 0 {
			aa = a.real[2]
			if na >= 2 { aa += Real.invRadix*a.real[3] }
			if na >= 3 { aa += Real.mprx2*a.real[4] }
			if na >= 4 { aa += Real.invRadix*Real.mprx2*a.real[5] }
			t1 = log2*Double(Real.NBT)*a.real[1]+log10(aa)
			if t1 >= 0 { nx = Int(t1) } else { nx = Int(t1-1) }
            Real.IntPower(&k.real, a: f, n: nx)  /* k = 10**nx */
            Real.Div(&k.real, a:a.real, b:k.real)  /* k = a*10**nx */
			
			/* adjust k if above is not quite right */
            while k.real[1] < 0 { nx -= 1; Real.Muld(&k.real, a: k.real, b: 10.0, n: 0) }
            while k.real[2] >= 10 { nx += 1; Real.Divd(&k.real, a: k.real, b: 10.0, n: 0) }
			k.real[0] = abs(k.real[0])
		} else {
            nx = 0; Real.Zero(&k.real)
		}
		
		/* force scientific notation for numbers too small or too large */
		Aexp = abs(nx)
		MaxExpWidth = ExpWidth
        if (ExpWidth == 0 && Aexp > Real.sigDigs) || (ExpWidth > 0) {
            /* force scientific notation */
            if Aexp > 999999     { ExpWidth  =  7 }
            else if Aexp > 99999 { ExpWidth  =  6 }
            else if Aexp > 9999  { ExpWidth  =  5 }
            else if Aexp > 999   { ExpWidth  =  4 }
            else if Aexp > 99    { ExpWidth  =  3 }
            else if Aexp > 9     { ExpWidth  =  2 }
            else                 { ExpWidth  =  1 }
        }
		if MaxExpWidth < ExpWidth { MaxExpWidth  =  ExpWidth }
		
		/* add the negative sign to the number */
		if ia < 0 { ConcatChar(&Str, "-") }
		
		/* ensure we don't exceed the maximum digits */
		FixPoint = Decimal != 0
		if (Decimal > Real.sigDigs) || !FixPoint {
			Decimal = Real.sigDigs-1
		}
		
		/* convert the number into scientific notation */
		if MaxExpWidth > 0 {
			GetDigit()                         /* leading digit */
			if EngFormat {
				while (Aexp % 3) != 0 {
					if nx < 0 {
						Aexp += 1; nx += 1
					} else {
						Aexp -= 1; nx -= 1
					}
					Decimal -= 1
					GetDigit()                      /* next digits */
				}
			}
			ConcatChar(&Str, ".")                    /* decimal point */
            for _ in 1...Decimal {
				GetDigit()                        /* add following digits */
			}
			Round(); Trim()          /* remove extraneous digits */
			
			/* add the exponent */
			ConcatChar(&Str, "E")
			if nx >= 0 {
				ConcatChar(&Str, "+")
			} else {
				ConcatChar(&Str, "-")
			}
			AddInt(&Str, abs(nx))
		} else {
			/* format a non-scientific number */
			if nx < 0 {
				ConcatChar(&Str, "0")                  /* leading digit */
				ConcatChar(&Str, ".");                  /* decimal point */
                for _ in stride(from: 2, through: abs(nx), by: 1) {      /* pad with leading zeros */
					ConcatChar(&Str, "0"); nl += 1
				}
				Decimal += nx+1
			}
			InCnt = 0
			repeat {
				GetDigit()
				if InCnt > nx {
					Decimal -= 1
				} else if InCnt == nx {
					ConcatChar(&Str, ".")
				}
				InCnt += 1
			} while !((InCnt == Real.sigDigs) || (Decimal == 0))
			
			/* remove any trailing zeros and unneeded digits */
			Round()
			Trim()
		}
		Real.curMantissa = nws
		return Str
	} //Format;

	func Sign () -> Int {
		let x = self
		if x.real[0] < 0 {
			return -1
		} else if x.real[0] > 0 {
			return 1
		} else {
			return 0
		}
	} //Sign;
	
	func isNegative() -> Bool  { self.real[0] < 0 }
	func isZero() -> Bool      { self.real[0] == 0 }
	func toString () -> String { self.toString(0, ExpWidth: 0, EngFormat: false) }

    /** Returns the closest Double equivalent.  if q is too large
    MAX(Double) is returned and if q is too small, zero
    is returned. */
	func Short () -> Double {
        var x = 0.0, exp = 0
        Real.RealToNumbExp(self.real, b: &x, n: &exp)
        return x * Real.ipower(2, base: exp)
	} //Short;

    /**
    Return the largest integer not greater than *self*.
    For example: Int(3.6) = 3 and Int(-1.6)=-2
    */
	func Entier () -> Real {
		var r = Real(size: Real.curMantissa+4)
        Real.Entier(&r.real, a: self.real)
		return r
	} //Entier;
	
    /**
    Return the fractional part of *self*.
    */
	func Fraction () -> Real {
		var r = self.Entier()
        if self.real[0] < 0 { Real.Add(&r.real, a:self.real, b:Real.One) }
        Real.Sub(&r.real, a:self.real, b:r.real)
		return r
	} //Fraction;

	/*---------------------------------------------------------*/
	/* Basic math routines                                     */
	
	func Add (_ z: Real) -> Real {
		var r = Real(size: Real.curMantissa+4)
        Real.Add(&r.real, a:self.real, b:z.real)
		return r
	} //Add;

	func Sub (_ z: Real) -> Real {
		var r = Real(size: Real.curMantissa+4)
        Real.Sub(&r.real, a:self.real, b:z.real)
		return r
	} //Sub;

	func Mul (_ z: Real) -> Real {
		var r = Real(size: Real.curMantissa+4)
        Real.Mul(&r.real, a:self.real, b:z.real)
		return r
	} //Mul;

	func Div (_ z: Real) -> Real {
		var r = Real(size: Real.curMantissa+4)
        Real.Div(&r.real, a:self.real, b:z.real)
		return r
	} //Div;
	
	func Abs () -> Real {
		/** returns the absolute value of z */
		var r = Real(size: Real.curMantissa+4)
        Real.Abs(&r.real, x:self.real)
		return r
	} //Abs;

	func Cmp (_ b: Real) -> Int {
		/**
		This routine compares the extended numbers `a' and `b' and
		returns the value -1, 0, or 1 dep}ing on whether `a'<`b',
		`a'=`b', or `a'>`b'.  It is faster than merely subtracting
		`a' and `b' and looking at the sign of the result.
		*/
        return Real.Cmp(self.real, b: b.real)
	} //Cmp;

	/*---------------------------------------------------------*/
	/* Power and transcendental routines                       */
	
    /** returns the value of the number x raised to the power exp */
	func Power (_ exp: Real) -> Real {
		var r = Real(size: Real.curMantissa+4)
		
		/* check for integer powers */
        Real.Entier(&r.real, a:exp.real)
        let n = Int(r.Short())
		if r.Cmp(exp) == 0 && abs(n) < 2000 {
            Real.IntPower(&r.real, a: self.real, n: n)
		} else { /* x^exp = Exp(exp*Ln(x)) */
            Real.Ln(&r.real, a:self.real); Real.Mul(&r.real, a:exp.real, b:r.real); Real.Exp(&r.real, a: r.real)
		}
		return r
	} //Power;
	
	func IRoot (_ n: Int) -> Real {
		/** returns the `n'th root of `z' */
		var r = Real(size: Real.curMantissa+4)
        Real.Root(&r.real, a: self.real, n: n)
		return r
	} //IRoot;
	
	func Sqrt () -> Real {
		/** Pre: z>=0. returns the square root of z */
		var r = Real(size: Real.curMantissa+4)
        Real.Sqrt(&r.real, a: self.real)
		return r
	} //Sqrt;
	
	func Exp () -> Real {
		/** returns the exponential of z */
		var r = Real(size: Real.curMantissa+4)
        Real.Exp(&r.real, a: self.real)
		return r
	} //Exp;
	
	func Ln () -> Real {
		/** returns the natural logarithm of z */
		var r = Real(size: Real.curMantissa+4)
        Real.Ln(&r.real, a: self.real)
		return r
	} //Ln;
	
	func Log (_ base: Real) -> Real {
		/** returns the 'base' logarithm of z */
		var r = Real(size: Real.curMantissa+4)
		var t = Real(size: Real.curMantissa+4)
        Real.Ln(&r.real, a: self.real); Real.Ln(&t.real, a:base.real); Real.Div(&r.real, a:r.real, b:t.real)
		return r
	} //Log;
	
	func Factorial () -> Real {
		/** returns the factorial of 'n'. */
		let MAXFACT = 388006  /* Limit of 1.50339063E+1999997 */
		var f = Real(size: Real.curMantissa+4)
		var min: Int
		var n: Int = Int(self.Entier().Short())
		
		if (n < 0) || (n > MAXFACT) {
			Real.status = Status.IllegalArgument; return Real.zero  /* out of range */
		}
		if n < 2 {
			return Real.zero         /* 0! & 1! */
		} else if n >= 300000 {
            Real.copy(Real.fact300000.real, b:&f.real); min = 300000
		} else if n >= 200000 {
            Real.copy(Real.fact200000.real, b:&f.real); min = 200000
		} else if n >= 100000 {
            Real.copy(Real.fact100000.real, b:&f.real); min = 100000
		} else {
            Real.copy(Real.One, b:&f.real); min = 1
		}

		while n > min {
            Real.Muld(&f.real, a:f.real, b:Double(n), n:0)   /* f=f*x */
			n -= 1											/* x=x-1 */
		}
		return f
	} //Factorial;
	
	func Sin () -> Real {
		/** returns the sine of z */
		var s = Real(size: Real.curMantissa+4)
		var c = Real(size: Real.curMantissa+4)
        Real.Zero(&c.real)
        Real.SinCos(&s.real, cos: &c.real, a:self.real)
		return s
	} //Sin;
	
	func Cos () -> Real {
		/** returns the cosine of z */
		var s = Real(size: Real.curMantissa+4)
		var c = Real(size: Real.curMantissa+4)
        Real.Zero(&s.real)
        Real.SinCos(&s.real, cos:&c.real, a:self.real)
		return c
	} //Cos;
	
	func SinCos (_ sin: inout Real, cos: inout Real) {
		/** returns the sine & cosine of z */
        Real.SinCos(&sin.real, cos:&cos.real, a:self.real)
	} //SinCos;
	
	func Tan () -> Real {
		/** returns the tangent of z */
		var s = Real(size: Real.curMantissa+4)
		var c = Real(size: Real.curMantissa+4)
		var r = Real(size: Real.curMantissa+4)
		
        Real.Zero(&s.real); Real.Zero(&c.real)
        Real.SinCos(&s.real, cos:&c.real, a:self.real); Real.Div(&r.real, a:s.real, b:c.real)
		return r
	} //Tan;
	
	func Arcsin () -> Real {
		/** returns the arcsine of z */
		var t = Real(size: Real.curMantissa+4)
		var r = Real(size: Real.curMantissa+4)
	
        Real.Abs(&t.real, x:self.real)
        if Real.Cmp(t.real, b: Real.One) > 0 {
			print("*** Illegal arcsin argument!"); Real.err = 20
		} else {
            Real.Mul(&t.real, a:t.real, b:t.real); Real.Sub(&t.real, a:Real.One, b:t.real); Real.Sqrt(&t.real, a: t.real)	/* t = Sqrt(1 - z^2) */
            Real.ATan2(&r.real, x: t.real, y: self.real)										/* r = ATan(z/Sqrt(1-z^2)) */
		}
		return r
	} //Arcsin;
	
	func Arccos () -> Real {
		/** returns the arccosine of z */
		var t = Real(size: Real.curMantissa+4)
		var r = Real(size: Real.curMantissa+4)

        Real.Abs(&t.real, x:self.real)
        if Real.Cmp(t.real, b: Real.One) > 0 {
			print("*** Illegal arccos argument!"); Real.err = 21
		} else {
            Real.Mul(&t.real, a:t.real, b:t.real); Real.Sub(&t.real, a:Real.One, b:t.real); Real.Sqrt(&t.real, a: t.real)	/* t = Sqrt(1 - z^2) */
            Real.ATan2(&r.real, x: self.real, y: t.real)										/* r = ATan(Sqrt(1-z^2)/z) */
		}
		return r
	} //Arccos;
	
	func Arctan () -> Real {
		/** returns the arctangent of z */
		var r = Real(size: Real.curMantissa+4)
        Real.ATan2(&r.real, x: Real.One, y: self.real)
		return r
	} //Arctan;
	
	func Arctan2 (_ xd: Real) -> Real {
		/** returns the arctangent of xn/xd */
		var r = Real(size: Real.curMantissa+4)
        Real.ATan2(&r.real, x:xd.real, y:self.real)
		return r
	} //Arctan2;
	
	func SinhCosh (_ sinh: inout Real, cosh: inout Real) {
		/** returns the hyberbolic sine & cosine of z */
        Real.SinhCosh(&sinh.real, cosh: &cosh.real, a: self.real)
	} //SinhCosh;
	
	func Sinh () -> Real {
		/** returns the hyperbolic sine of z */
		var s = Real(size: Real.curMantissa+4)
		var c = Real(size: Real.curMantissa+4)
        Real.SinhCosh(&s.real, cosh: &c.real, a: self.real)
		return s
	} //Sinh;
	
	func Cosh () -> Real {
		/** returns the hyperbolic cosine of z */
		var c = Real(size: Real.curMantissa+4)
		var s = Real(size: Real.curMantissa+4)
        Real.SinhCosh(&s.real, cosh: &c.real, a: self.real)
		return c
	} //Cosh;
	
	func Tanh () -> Real {
		/** returns the hyperbolic tangent of z */
		var sinh = Real(size: Real.curMantissa+4)
		var cosh = Real(size: Real.curMantissa+4)
		var r = Real(size: Real.curMantissa+4)
        Real.SinhCosh(&sinh.real, cosh: &cosh.real, a: self.real); Real.Div(&r.real, a:sinh.real, b:cosh.real)
		return r
	} //Tanh;

	func Random () -> Real {
		/** return a random number between 0 and 1 */
		var res = Real(size: Real.curMantissa+4)
		var t: Real
		if Real.Pi.count == 0 { Real.Initialize() }
        Real.Add(&res.real, a:Real.Seed.real, b:Real.Pi)
		t = res.Ln(); t = t.Mul(Real(5))
		t = t.Exp(); res = t; t = t.Entier()
        Real.Seed = res.Sub(t)
		return Real.Seed
	} //Random;

	static func Test() {
		
		func MakeReal (_ x: RealArray) -> Real { Real(x) }
		
		var s, n, m: Real
		
		Real.Initialize()
		Real.sigDigs = 100
		print("zero=\(Real.zero)")
		print("one=\(Real.one)")
        
        // 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940
        // 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211707
        // 0.69314718055994530941723212145817656807550013436025525412068000949339362196969471560586332699641868754200148102057068573368552023575813055703267075
        // 0.69314718055994530941723212145817656807550013436025525412068000949339362196969471560586332699641869
        // 1.41421356237309504880168872420969807856967187537694807317667973799073247846210703885038753432764157273501384623091229702492483605585073721264412149
        // 1.41421356237309504880168872420969807856967187537694807317667973799073247846210703885038753432764157
		print("pi=\(MakeReal(Real.Pi))")
		print("ln2=\(MakeReal(Real.Log2))")
		n = Real("123456789012345678901234567890123456789")
		m = Real("0.123456789012345678901234567890123456789")
		switch n.Cmp(m) {
		case 0: print("n=m")
		case 1: print("n>m")
		default: print("n<m")
		}
		print("n=\(n)")
		print("m=\(m)")
		s = n.Mul(m)
		print("n*m=\(s)")
		s = n.Add(m)
		print("n+m=\(s)")
        s = n.Sub(m)
		print("n-m=\(s)")
		s = n.Div(m)
		print("n/m=\(s)")
		n = Real(1)
		s = n.Div(Real(3));
		print("1/3=\(s)")
		print("1/3+1/3=\(s.Add(s))")
		print("1/3*1/3=\(s.Mul(s))")
		print("1/3*3=\(s.Mul(Real(3)))")
		n = Real(2)
		s = n.Power(Real(64))
		print("2^64=\(s)")
        print("sqrt(2)=\(n.Sqrt())")
		n = Real("1.010E-10")
		print("1.010E-10=\(n)")
		n = Real("-12.0E+10")
		print("-12.0E+10=\(n)")
		n = Real("0.00045E-10")
		print("0.00045E-10=\(n)")
		n = Real("-12 345 678")
		print("-12 345 678=\(n)")
		n = Real("1E10000")
		print("1E10000=\(n)")
		Real.pi.SinCos(&m, cos: &n)
		print("Sin(pi)=\(m)")
		print("Cos(pi)=\(n)")
		m = Real.pi.Div(Real(8))
		m.SinCos(&m, cos: &n)
        // 0.38268343236508977172845998403039886676134456248562704143380063562754603396008969223701378534228355
        // 0.38268343236508977172845998403039886676134456248562704143380063562754603396008969223701378534228354714842428866149355590075601020096759792084420917
        // 0.9238795325112867561281831893967882868224166258636424861150977312805350075011023587148399348503446
        // 0.92387953251128675612818318939678828682241662586364248611509773128053500750110235871483993485034459609796302578224788303086917757990420142753322199
		print("Sin(pi/8)=\(m)")
		print("Cos(pi/8)=\(n)")
		one.SinCos(&m, cos: &n)
        // 0.8414709848078965066525023216302989996225630607983710656727517099919104043912396689486397435430527
        // 0.84147098480789650665250232163029899962256306079837106567275170999191040439123966894863974354305269585434903790792067429325911892099189888119341032
        // 0.54030230586813971740093660744297660373231042061792222767009725538110039477447176451795185608718309
        // 0.54030230586813971740093660744297660373231042061792222767009725538110039477447176451795185608718308934357173116003008909786063376002166345640651226
		print("Sin(1)=\(m)")
		print("Cos(1)=\(n)")
		m = Real(-8)
		print("-8^(-1/3)=\(m.IRoot(3))")
		m = Real(2); m = m.Power(Real(64));
		print("(2^64)^(-1/64)=\(m.IRoot(64))")
		m = Real(4)
		print("4*arctan(1)=\(m.Mul(Real.one.Arctan()))")
		m = Real.one.Sin()
		print("arcsin(sin(1))=\(m.Arcsin())")
		m = Real.one.Cos()
		print("arccos(cos(1))=\(m.Arccos())")
		m = Real(3.6)
		print("Entier(3.6)=\(m.Entier())")
		m = Real(-3.6)
		print("Entier(-3.6)=\(m.Entier())")
        m = Real(69)
        print("69!=\(m.Factorial())")
        print("Test Complete!")
	} //Test;

	private static func Initialize () {
		var t0 = RealArray(repeating: 0, count:2*(Real.maxMant+4))
        var t1 = RealArray(repeating: 0, count:2*(Real.maxMant+4))
        var t2 = RealArray(repeating: 0, count:2*(Real.maxMant+4))
        var t3 = RealArray(repeating: 0, count:2*(Real.maxMant+4))
        var t4 = RealArray(repeating: 0, count:2*(Real.maxMant+4))
//		var x1 = RealArray(arrayLiteral: 1, 0, 1)
		
		/* initialize internal constants */
		Real.err = 0; Real.curMantissa = Real.maxMant+1; Real.debug = 0; Real.numBits = 22
		print("Initializing...")
		
		/************************* TEMPORARY ******************/
//		Real.debug = 10
		
		/* compute required constants to correct precision */
        Real.Zero(&t1); Real.Pi(&t1)						/* t1 = pi */
		NumbExpToReal(2, n: 0, b: &t0)			/* t0 = 2.0 */
		Ln(&t2, a: t0)							/* t2 = Ln(2.0) */
		Real.Log2 = RealArray(repeating:0, count: t2.count)
        Real.copy(t2, b:&Real.Log2)
		NumbExpToReal(10, n: 0, b: &t0)			/* t0 = 10.0 */
        Real.Ln(&t3, a: t0)							/* t3 = Ln(10.0) */
        Real.IntPower(&t4, a: t0, n: Real.log10eps)  /* t4 = 10^(10-maxDigits) */
	
		/* transfer to current variables */
		Real.curMantissa = Real.maxMant
        Real.Pi = RealArray(repeating:0, count: Real.curMantissa+4); copy(t1, b:&Real.Pi)
		Real.Log2 = RealArray(repeating:0, count: Real.curMantissa+4); copy(t2, b:&Real.Log2)
//		Real.ln10 = RealArray(count: Real.curMantissa+4, repeatedValue:0); copy(t3, b:&Real.ln10)
//		Real.eps = RealArray(count: Real.curMantissa+4, repeatedValue:0); copy(t4, b:&Real.eps)
		
		/* set the current output precision */
		Real.sigDigs = Real.maxDigits
	} //Init;

} //Reals.

extension Real : Comparable {
    static func < (lhs: Real, rhs: Real) -> Bool { lhs.Cmp(rhs) == -1 }
}

