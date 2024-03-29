//
//  complex.swift
//  complex
//
//  Created by Dan Kogai on 6/12/14.
//  Copyright (c) 2014 Dan Kogai. All rights reserved.
//

import Foundation
// protocol RealType : FloatingPointType // sadly crashes as of Swift 1.1 :-(
//public protocol RealType {
//    // copied from FloatingPointType
//    init(_ value: UInt8)
//    init(_ value: Int8)
//    init(_ value: UInt16)
//    init(_ value: Int16)
//    init(_ value: UInt32)
//    init(_ value: Int32)
//    init(_ value: UInt64)
//    init(_ value: Int64)
//    init(_ value: UInt)
//    init(_ value: Int)
//    init(_ value: Double)
//    init(_ value: Float)
//    
//    // for StringLiteralConvertible support
//    init?(_ value: String)
//    
//    // class vars are now gone 
//    // because they will be static vars in Swift 1.2, 
//    // making them incompatible to one another
////    static var infinity: Self { get }
////    static var NaN: Self { get }
////    static var quietNaN: Self { get }
//    
//    var floatingPointClass: FloatingPointClassification { get }
//    var isSignMinus: Bool { get }
//    var isNormal: Bool { get }
//    var isFinite: Bool { get }
//    var isZero: Bool { get }
//    var isSubnormal: Bool { get }
//    var isInfinite: Bool { get }
//    var isNaN: Bool { get }
//    var isSignaling: Bool { get }
//    // copied from Hashable
//    var hashValue: Int { get }
//    // Built-in operators
//    static func ==(_: Self, _: Self)->Bool
//    static func !=(_: Self, _: Self)->Bool
//    static func < (_: Self, _: Self)->Bool
//    static func <= (_: Self, _: Self)->Bool
//    static func > (_: Self, _: Self)->Bool
//    static func >= (_: Self, _: Self)->Bool
//    static prefix func + (_: Self)->Self
//    static prefix func - (_: Self)->Self
//    static func + (_: Self, _: Self)->Self
//    static func - (_: Self, _: Self)->Self
//    static func * (_: Self, _: Self)->Self
//    static func / (_: Self, _: Self)->Self
//    static func += ( _: inout Self, _: Self)
//    static func -= ( _: inout Self, _: Self)
//    static func *= ( _: inout Self, _: Self)
//    static func /= ( _: inout Self, _: Self)
//    // methodized functions for protocol's sake
//    var abs:Self { get }
//    func cos()->Self
//    func exp()->Self
//    func log()->Self
//    func sin()->Self
//    func sqrt()->Self
//    func hypot(_: Self)->Self
//    func atan2(_: Self)->Self
//    func pow(_: Self)->Self
//    
////    static var LN10:Self { get }
////    static var epsilon:Self { get }
//}
// Double is default since floating-point literals are Double by default
//extension Double : RealType {
//    public var abs:Double { return Swift.abs(self) }
//    public func cos()->Double { return Foundation.cos(self) }
//    public func exp()->Double { return Foundation.exp(self) }
//    public func log()->Double { return Foundation.log(self) }
//    public func sin()->Double { return Foundation.sin(self) }
//    public func sqrt()->Double { return Foundation.sqrt(self) }
//    public func atan2(_ y:Double)->Double { return Foundation.atan2(self, y) }
//    public func hypot(_ y:Double)->Double { return Foundation.hypot(self, y) }
//    public func pow(_ y:Double)->Double { return Foundation.pow(self, y) }
//    // these ought to be static let
//    // but give users a chance to overwrite it
//    static var PI = 3.14159265358979323846264338327950288419716939937510
//    static var π = PI
//    static var E =  2.718281828459045235360287471352662497757247093699
//    static var e = E
//    static var LN2 = 0.6931471805599453094172321214581765680755001343602552
//    static var LOG2E = 1 / LN2
//    static var LN10 = 2.3025850929940456840179914546843642076011014886287729
//    static var LOG10E = 1/LN10
//    static var SQRT2 = 1.4142135623730950488016887242096980785696718753769480
//    static var SQRT1_2 = 1/SQRT2
//    static var epsilon = 0x1p-52
//    /// self * 1.0i
//    var i:Complex<Double>{ return Complex<Double>(0.0, self) }
//}
//// But when explicitly typed you can use Float
//extension Float : RealType {
//    public var abs:Float { return Swift.abs(self) }
//    public func cos()->Float { return Foundation.cos(self) }
//    public func exp()->Float { return Foundation.exp(self) }
//    public func log()->Float { return Foundation.log(self) }
//    public func sin()->Float { return Foundation.sin(self) }
//    public func sqrt()->Float { return Foundation.sqrt(self) }
//    public func hypot(_ y:Float)->Float { return Foundation.hypot(self, y) }
//    public func atan2(_ y:Float)->Float { return Foundation.atan2(self, y) }
//    public func pow(_ y:Float)->Float { return Foundation.pow(self, y) }
//    // these ought to be static let
//    // but give users a chance to overwrite it
//    static var PI:Float = 3.14159265358979323846264338327950288419716939937510
//    static var π:Float = PI
//    static var E:Float =  2.718281828459045235360287471352662497757247093699
//    static var e:Float = E
//    static var LN2:Float = 0.6931471805599453094172321214581765680755001343602552
//    static var LOG2E:Float = 1 / LN2
//    static var LN10:Float = 2.3025850929940456840179914546843642076011014886287729
//    static var LOG10E:Float = 1/LN10
//    static var SQRT2:Float = 1.4142135623730950488016887242096980785696718753769480
//    static var SQRT1_2:Float = 1/SQRT2
//    static var epsilon:Float = 0x1p-23
//    /// self * 1.0i
//    var i:Complex<Float>{ return Complex<Float>(0.0 as Float, self) }
//}
// el corazon
//struct Complex<T:RealType> : Equatable, Hashable {
//	var re:T
//	var im:T
//
//    init(_ re:T, _ im:T) {
//        self.re = re
//        self.im = im
//    }
//    init(){ self.init(T(0), T(0)) }
//    init(abs:T, arg:T) {
//        self.re = abs * arg.cos()
//        self.im = abs * arg.sin()
//    }
//    /// real part thereof
//    var real:T { get{ return re } set(r){ re = r } }
//    /// imaginary part thereof
//    var imag:T { get{ return im } set(i){ im = i } }
//    /// absolute value thereof
//    var abs:T {
//        get { return re.hypot(im) }
//        set(r){ let f = r / abs; re *= f; im *= f }
//    }
//    /// argument thereof
//    var arg:T  {
//        get { return im.atan2(re) }
//        set(t){ let m = abs; re = m * t.cos(); im = m * t.sin() }
//    }
//    /// norm thereof
//    var norm:T { return re.hypot(im) }
//    /// conjugate thereof
//    var conj:Complex { return Complex(re, -im) }
//    /// projection thereof
//    var proj:Complex {
//        if re.isFinite && im.isFinite {
//            return self
//        } else {
//            return Complex(
//                T(1)/T(0), im.isSignMinus ? -T(0) : T(0)
//            )
//        }
//    }
//    /// (real, imag)
//    var tuple:(T, T) {
//        get { return (re, im) }
//        set(t){ (re, im) = t}
//    }
//    /// z * i
//    var i:Complex { return Complex(-im, re) }
//    /// .description -- conforms to Printable
////    var description:String {
////        let plus = im.isSignMinus ? "" : "+"
////        return "(\(re)\(plus)\(im).i)"
////    }
//    /// .hashvalue -- conforms to Hashable
////    var hashValue:Int { // take most significant halves and join
////        let bits = MemoryLayout<Int>.size * 4
////        let mask = bits == 16 ? 0xffff : 0xffffFFFF
////        return (re.hashValue & ~mask) | (im.hashValue >> bits)
////    }
//}
//// operator definitions
//
//infix operator ** : ExponentPrecedence
//precedencegroup ExponentPrecedence {
//    associativity: left
//    higherThan: MultiplicationPrecedence
//}
//infix operator **= : AssignmentPrecedence
//infix operator =~ : ComparisonPrecedence
//infix operator !~ : ComparisonPrecedence
//
//// != is auto-generated thanks to Equatable
//func == <T>(lhs:Complex<T>, rhs:Complex<T>) -> Bool {
//    return lhs.re == rhs.re && lhs.im == rhs.im
//}
//func == <T>(lhs:Complex<T>, rhs:T) -> Bool {
//    return lhs.re == rhs && lhs.im == T(0)
//}
//func == <T>(lhs:T, rhs:Complex<T>) -> Bool {
//    return rhs.re == lhs && rhs.im == T(0)
//}
//// +, +=
//prefix func + <T>(z:Complex<T>) -> Complex<T> {
//    return z
//}
//func + <T>(lhs:Complex<T>, rhs:Complex<T>) -> Complex<T> {
//    return Complex(lhs.re + rhs.re, lhs.im + rhs.im)
//}
//func + <T>(lhs:Complex<T>, rhs:T) -> Complex<T> {
//    return lhs + Complex(rhs, T(0))
//}
//func + <T>(lhs:T, rhs:Complex<T>) -> Complex<T> {
//    return Complex(lhs, T(0)) + rhs
//}
//func += <T>( lhs:inout Complex<T>, rhs:Complex<T>) {
//    lhs.re += rhs.re ; lhs.im += rhs.im
//}
//func += <T>( lhs:inout Complex<T>, rhs:T) {
//    lhs.re += rhs
//}
//// -, -=
//prefix func - <T>(z:Complex<T>) -> Complex<T> {
//    return Complex<T>(-z.re, -z.im)
//}
//func - <T>(lhs:Complex<T>, rhs:Complex<T>) -> Complex<T> {
//    return Complex(lhs.re - rhs.re, lhs.im - rhs.im)
//}
//func - <T>(lhs:Complex<T>, rhs:T) -> Complex<T> {
//    return lhs - Complex(rhs, T(0))
//}
//func - <T>(lhs:T, rhs:Complex<T>) -> Complex<T> {
//    return Complex(lhs, T(0)) - rhs
//}
//func -= <T>( lhs:inout Complex<T>, rhs:Complex<T>) {
//    lhs.re -= rhs.re ; lhs.im -= rhs.im
//}
//func -= <T>( lhs:inout Complex<T>, rhs:T) {
//    lhs.re -= rhs
//}
//// *, *=
//func * <T>(lhs:Complex<T>, rhs:Complex<T>) -> Complex<T> {
//    return Complex(
//        lhs.re * rhs.re - lhs.im * rhs.im,
//        lhs.re * rhs.im + lhs.im * rhs.re
//    )
//}
//func * <T>(lhs:Complex<T>, rhs:T) -> Complex<T> {
//    return Complex(lhs.re * rhs, lhs.im * rhs)
//}
//func * <T>(lhs:T, rhs:Complex<T>) -> Complex<T> {
//    return Complex(lhs * rhs.re, lhs * rhs.im)
//}
//func *= <T>( lhs: inout Complex<T>, rhs:Complex<T>) {
//    lhs = lhs * rhs
//}
//func *= <T>( lhs: inout Complex<T>, rhs:T) {
//    lhs = lhs * rhs
//}
//// /, /=
////
//// cf. https://github.com/dankogai/swift-complex/issues/3
////
//func / <T>(lhs:Complex<T>, rhs:Complex<T>) -> Complex<T> {
//    if rhs.re.abs >= rhs.im.abs {
//        let r = rhs.im / rhs.re
//        let d = rhs.re + rhs.im * r
//        return Complex (
//            (lhs.re + lhs.im * r) / d,
//            (lhs.im - lhs.re * r) / d
//        )
//    } else {
//        let r = rhs.re / rhs.im
//        let d = rhs.re * r + rhs.im
//        return Complex (
//            (lhs.re * r + lhs.im) / d,
//            (lhs.im * r - lhs.re) / d
//        )
//
//    }
//}
//func / <T>(lhs:Complex<T>, rhs:T) -> Complex<T> {
//    return Complex(lhs.re / rhs, lhs.im / rhs)
//}
//func / <T>(lhs:T, rhs:Complex<T>) -> Complex<T> {
//    return Complex(lhs, T(0)) / rhs
//}
//func /= <T>( lhs: inout Complex<T>, rhs:Complex<T>) {
//    lhs = lhs / rhs
//}
//func /= <T>( lhs: inout Complex<T>, rhs:T) {
//    lhs = lhs / rhs
//}
//// exp(z)
//func exp<T>(_ z:Complex<T>) -> Complex<T> {
//    let abs = z.re.exp()
//    let arg = z.im
//    return Complex(abs * arg.cos(), abs * arg.sin())
//}
//// log(z)
//func log<T>(_ z:Complex<T>) -> Complex<T> {
//    return Complex(z.abs.log(), z.arg)
//}
//// log10(z) -- just because C++ has it
//func log10<T>(_ z:Complex<T>) -> Complex<T> { return log(z) / T(log(10.0)) }
//func log10<T:RealType>(_ r:T) -> T { return r.log() / T(log(10.0)) }
//// pow(b, x)
//func pow<T>(_ lhs:Complex<T>, _ rhs:Complex<T>) -> Complex<T> {
//    if lhs == T(0) { return Complex(T(1), T(0)) } // 0 ** 0 == 1
//    let z = log(lhs) * rhs
//    return exp(z)
//}
//func pow<T>(_ lhs:Complex<T>, _ rhs:T) -> Complex<T> {
//    return pow(lhs, Complex(rhs, T(0)))
//}
//func pow<T>(_ lhs:T, _ rhs:Complex<T>) -> Complex<T> {
//    return pow(Complex(lhs, T(0)), rhs)
//}
//// **, **=
//func ** <T:RealType>(lhs:T, rhs:T) -> T {
//    return lhs.pow(rhs)
//}
//func ** <T>(lhs:Complex<T>, rhs:Complex<T>) -> Complex<T> {
//    return pow(lhs, rhs)
//}
//func ** <T>(lhs:T, rhs:Complex<T>) -> Complex<T> {
//    return pow(lhs, rhs)
//}
//func ** <T>(lhs:Complex<T>, rhs:T) -> Complex<T> {
//    return pow(lhs, rhs)
//}
//func **= <T:RealType>( lhs: inout T, rhs:T) {
//    lhs = lhs.pow(rhs)
//}
//func **= <T>( lhs: inout Complex<T>, rhs:Complex<T>) {
//    lhs = pow(lhs, rhs)
//}
//func **= <T>( lhs: inout Complex<T>, rhs:T) {
//    lhs = pow(lhs, rhs)
//}
//// sqrt(z)
//func sqrt<T>(_ z:Complex<T>) -> Complex<T> {
//    // return z ** 0.5
//    let d = z.re.hypot(z.im)
//    let re = ((z.re + d)/T(2)).sqrt()
//    if z.im < T(0) {
//        return Complex(re, -((-z.re + d)/T(2)).sqrt())
//    } else {
//        return Complex(re,  ((-z.re + d)/T(2)).sqrt())
//    }
//}
//// cos(z)
//func cos<T>(_ z:Complex<T>) -> Complex<T> {
//    // return (exp(i*z) + exp(-i*z)) / 2
//    return (exp(z.i) + exp(-z.i)) / T(2)
//}
//// sin(z)
//func sin<T>(_ z:Complex<T>) -> Complex<T> {
//    // return (exp(i*z) - exp(-i*z)) / (2*i)
//    return -(exp(z.i) - exp(-z.i)).i / T(2)
//}
//// tan(z)
//func tan<T>(_ z:Complex<T>) -> Complex<T> {
//    // return sin(z) / cos(z)
//    let ezi = exp(z.i), e_zi = exp(-z.i)
//    return (ezi - e_zi) / (ezi + e_zi).i
//}
//// atan(z)
//func atan<T>(_ z:Complex<T>) -> Complex<T> {
//    let l0 = log(T(1) - z.i), l1 = log(T(1) + z.i)
//    return (l0 - l1).i / T(2)
//}
//func atan<T:RealType>(_ r:T) -> T { return atan(Complex(r, T(0))).re }
//// atan2(z, zz)
//func atan2<T>(_ z:Complex<T>, zz:Complex<T>) -> Complex<T> {
//    return atan(z / zz)
//}
//// asin(z)
//func asin<T>(_ z:Complex<T>) -> Complex<T> {
//    return -log(z.i + sqrt(T(1) - z*z)).i
//}
//// acos(z)
//func acos<T>(_ z:Complex<T>) -> Complex<T> {
//    return log(z - sqrt(T(1) - z*z).i).i
//}
//// sinh(z)
//func sinh<T>(_ z:Complex<T>) -> Complex<T> {
//    return (exp(z) - exp(-z)) / T(2)
//}
//// cosh(z)
//func cosh<T>(_ z:Complex<T>) -> Complex<T> {
//    return (exp(z) + exp(-z)) / T(2)
//}
//// tanh(z)
//func tanh<T>(_ z:Complex<T>) -> Complex<T> {
//    let ez = exp(z), e_z = exp(-z)
//    return (ez - e_z) / (ez + e_z)
//}
//// asinh(z)
//func asinh<T>(_ z:Complex<T>) -> Complex<T> {
//    return log(z + sqrt(z*z + T(1)))
//}
//// acosh(z)
//func acosh<T>(_ z:Complex<T>) -> Complex<T> {
//    return log(z + sqrt(z*z - T(1)))
//}
//// atanh(z)
//func atanh<T>(_ z:Complex<T>) -> Complex<T> {
//	let one = T(1)
//    let t = log((one + z)/(one - z))
//    return t / T(2)
//}
//// for the compatibility's sake w/ C++11
//func abs<T>(_ z:Complex<T>) -> T { return z.abs }
//func arg<T>(_ z:Complex<T>) -> T { return z.arg }
//func real<T>(_ z:Complex<T>) -> T { return z.real }
//func imag<T>(_ z:Complex<T>) -> T { return z.imag }
//func norm<T>(_ z:Complex<T>) -> T { return z.norm }
//func conj<T>(_ z:Complex<T>) -> Complex<T> { return z.conj }
//func proj<T>(_ z:Complex<T>) -> Complex<T> { return z.proj }
////
//// approximate comparisons
////
//func =~ <T:RealType>(lhs:T, rhs:T) -> Bool {
//    if lhs == rhs { return true }
//    let t = (rhs - lhs) / rhs
//    let epsilon = MemoryLayout<T>.size < 8 ? 0x1p-23 : 0x1p-52
//    return t.abs <= T(2) * T(epsilon)
//}
//func =~ <T>(lhs:Complex<T>, rhs:Complex<T>) -> Bool {
//    if lhs == rhs { return true }
//    return lhs.abs =~ rhs.abs
//}
//func =~ <T>(lhs:Complex<T>, rhs:T) -> Bool {
//    return lhs.abs =~ rhs.abs
//}
//func =~ <T>(lhs:T, rhs:Complex<T>) -> Bool {
//    return lhs.abs =~ rhs.abs
//}
//func !~ <T:RealType>(lhs:T, rhs:T) -> Bool {
//    return !(lhs =~ rhs)
//}
//func !~ <T>(lhs:Complex<T>, rhs:Complex<T>) -> Bool {
//    return !(lhs =~ rhs)
//}
//func !~ <T>(lhs:Complex<T>, rhs:T) -> Bool {
//    return !(lhs =~ rhs)
//}
//func !~ <T>(lhs:T, rhs:Complex<T>) -> Bool {
//    return !(lhs =~ rhs)
//}


