//
//  ViewController.swift
//  EngineerMath
//
//  Created by Michael Griebling on 22May2015.
//  Copyright (c) 2015 Solinst Canada. All rights reserved.
//

import Cocoa

class ViewController: NSViewController {

	override func viewDidLoad() {
		super.viewDidLoad()

		// Do any additional setup after loading the view.
//		var a = NSDecimalNumber(string: "12.345E50")
//		println("a = \(a), \nmax = \(NSDecimalNumber.maximumDecimalNumber()), \nmin = \(NSDecimalNumber.minimumDecimalNumber())")
//		let b = NSDecimalNumber.one().decimalNumberByDividingBy(a)
//		println("1/a = \(b)")
		
		// BigCFloat
////		var x : BigReal = 1
////		var x = BigReal(1)
//		var x : BigComplex = 1
//		var y : BigComplex = 2.3456-2.5*i
//		var z : BigComplex = "+5 - i23"
		
////		println("atan \(y)/\(x) = \(x.atan2(y))")
//		println("x = \(x), y = \(y)")
//		println("z = \(z); 1/z = \(1/z)")
//		println("\(x) + \(y) = \(x+y)")
//		println("\(x) - \(y) = \(x-y)")
//		println("\(x) * \(y) = \(x*y)")
//		println("\(x) / \(y) = \(x/y)")
//		println("\(x) ** 2 = \(x ** 2)")
//		println("\(x) ** 3 = \(x ** 3)")
//		println("\(y) ** 2 = \(y ** 2)")
//		println("\(y) ** 3 = \(y ** 3)")
//		println("\(x) ** -1 = \(x ** -1)")
//		println("2 ** 1000 = \(2 ** 1000)")
//		if x > y { println("\(x) > \(y)") }
//		else if x < y { println("\(x) < \(y)") }
//		else { println("\(x) = \(y)") }
        
        var xa = [1,2,3,4]
        let a = xa[safe: 6]
        print(a, xa)
        
        typealias V = Measurement<Unit>.Voltage
        typealias R = Measurement<Unit>.Resistance
        typealias I = Measurement<Unit>.Current
        typealias T = Measurement<Unit>.Duration
		
        let v = V(10, .volts)
        let r = R(20, .ohms)
        let f = 0.10000000000000003
        let x = v / r   // result unit is amperes
        let y = v * x   // result unit is watts
        let i = I(6, .microamperes)
        let t = T(52*10, .weeks)  // 10 years
        let e = i * t   // result unit is amp-hours
        let ly = Measurement(1, UnitLength.lightyears)
        let t1 = Measurement(value:25, unit: UnitTemperature.celsius)
        let t2 = Measurement(value:10, unit: UnitTemperature.fahrenheit)
        let t3 = t1 + t2
        let t1k = t1.converted(to: .kelvin)
        let t2k = t2.converted(to: .kelvin)
        print(t1, " or ", t1k, " + ", t2, " or ", t2k, " = ", t3, " or ", t3.converted(to: .celsius))
//        let i = UnitSpeed.kilometersPerHour.symbol
//        let myOhm = UnitElectricResistance.ohms
//        let x = t == s
        print(f, v, " / ", r, " = ", x, ", ", v, " * ", x, " = ", y, ", ", i, " for ", t, " = ", e)
        print(ly, " = ", ly.converted(to: .kilometers))
        
//        t.add("m"); t.add("m")
//        t.add("ft")
//        t.removeAll("m")
        
//        print("Bag contains \(t.count) items: ")
//        for (index, item) in t.enumerated() {
//            print("\(item)", terminator: "")
//            if index < t.uniqueCount-1 { print(", ", terminator: "") }
//        }
//        print()
		
		// Test unit conversions
//		let hour = Units("hr")
//		let miphr = Units("mi/hr")
//		let kmphr = Units("km/hr")
//		let mps = Units("m/s")
//		let ftps = Units("ft/s")
//		let seconds = Units("s")
//		let mile = Units("mi")
//		let km = Units("km")
//		let degreeC = Units("°C")
//		let degreeF = Units("°F")
//		let kelvin = Units("K")
//		let kg = Units("kg")
//		let lb = Units("lb")
//		let mA = Units("mA")
//		let Amp = Units("A")
//		let volt = Units("V")
//		let mV = Units("mV")
//		let lightYear = Units("ly")
////		let c = Units("c")
//		var result = Units.convert(10, fromType: hour, toType: seconds)
//		print("10 \(hour) = \(result!) \(seconds)")
//		result = Units.convert(10, fromType:mile, toType: km)
//		print("10 \(mile) = \(result!) \(km)")
//		result = Units.convert(60, fromType:miphr, toType: kmphr)
//		print("60 \(miphr) = \(result!) \(kmphr)")
//		result = Units.convert(5, fromType:mps, toType: kmphr)
//		print("5 \(mps) = \(result!) \(kmphr)")
//		result = Units.convert(5, fromType:ftps, toType: kmphr)
//		print("5 \(ftps) = \(result!) \(kmphr)")
//		result = Units.convert(25, fromType:degreeC, toType: kelvin)
//		print("25 \(degreeC) = \(result!) \(kelvin)")
//		result = Units.convert(25, fromType:degreeC, toType: degreeF)
//		print("25 \(degreeC) = \(result!) \(degreeF)")
//		result = Units.convert(200, fromType:lb, toType: kg)
//		print("200 \(lb) = \(result!) \(kg)")
//		print("200 \(mA) = \(Units.convert(200, fromType:mA, toType: Amp)!) \(Amp)")
//		print("200 \(mV) = \(Units.convert(200, fromType:mV, toType: volt)!) \(volt)")
//		print("V/A = \(volt/Amp)")
//		print("1 \(lightYear) = \(Units.convert(1, fromType:lightYear, toType: km)!) \(km)")
////		print("1 \(c) = \(Units.convert(1, fromType:c, toType: kmphr)!) \(kmphr)")
//        print("unsigned long size = \(sizeof(Int))")
//        Real.Test()
//        let x : RComplex = "1+2i"
//        let y = RComplex("2 - 10.5i")
//        print("x=\(x), y=\(y), x+y=\(x+y), x*y=\(x*y),\nx/y = \(x/y)")
	}

//    override var representedObject: AnyObject? {
//        didSet {
//        // Update the view, if already loaded.
//        }
//    }


}

