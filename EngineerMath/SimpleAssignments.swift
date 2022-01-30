//
//  SimpleAssignments.swift
//  EngineerMath
//
//  Created by Michael Griebling on 2018-05-28.
//  Copyright © 2018 Computer Inspirations. All rights reserved.
//

import Foundation

public protocol IntCompatible : BinaryInteger {}

extension Int8 : IntCompatible {}
extension Int16 : IntCompatible {}
extension Int32 : IntCompatible {}
extension UInt8 : IntCompatible {}
extension UInt16 : IntCompatible {}
extension UInt32 : IntCompatible {}

public func + <T:IntCompatible>(l: Int, r: T) -> Int { l + Int(r) }
public func + <T:IntCompatible>(l: T, r: Int) -> Int { Int(l) + r }
public func - <T:IntCompatible>(l: Int, r: T) -> Int { l - Int(r) }
public func - <T:IntCompatible>(l: T, r: Int) -> Int { Int(l) - r }
public func * <T:IntCompatible>(l: Int, r: T) -> Int { l * Int(r) }
public func * <T:IntCompatible>(l: T, r: Int) -> Int { Int(l) * r }
public func / <T:IntCompatible>(l: Int, r: T) -> Int { l / Int(r) }
public func / <T:IntCompatible>(l: T, r: Int) -> Int { Int(l) / r }

extension Double {
    static func + <T:IntCompatible>(l: Double, r: T) -> Double { l + Double(Int(r)) }
    static func + <T:IntCompatible>(l: T, r: Double) -> Double { Double(Int(l)) + r }
}

