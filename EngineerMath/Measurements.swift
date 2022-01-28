//
//  Measurements.swift
//  EngineerMath
//
//  Created by Mike Griebling on 14 Apr 2018.
//  Copyright © 2018 Solinst Canada. All rights reserved.
//

import Foundation

/// Measurement extension to support operations like multiplication and division
extension Measurement {
    
    ///////////////////////////////////////////////////////////////////////////////
    // Aliases to make the code more readable
    public typealias Speed = Measurement<UnitSpeed>
    public typealias Length = Measurement<UnitLength>
    public typealias Duration = Measurement<UnitDuration>
    public typealias Area = Measurement<UnitArea>
    public typealias Volume = Measurement<UnitVolume>
    public typealias Current = Measurement<UnitElectricCurrent>
    public typealias Voltage = Measurement<UnitElectricPotentialDifference>
    public typealias Resistance = Measurement<UnitElectricResistance>
    public typealias Power = Measurement<UnitPower>
    public typealias Acceleration = Measurement<UnitAcceleration>
    public typealias Charge = Measurement<UnitElectricCharge>
    public typealias Energy = Measurement<UnitEnergy>
    
    ///////////////////////////////////////////////////////////////////////////////
    // MARK: - Convenience initializer to simplify creation.
    
    public init(_ x : Double, _ unit : UnitType) {
        self.init(value: x, unit: unit)
    }
    
    ///////////////////////////////////////////////////////////////////////////////
    // MARK: - Handle speed-related expressions
    
    /// Support ab = a * b where `a` is duration, `b` is speed, and `ab` is length
    public static func * (a: Duration, b: Speed) -> Length {
        let b = b.converted(to: UnitSpeed.baseUnit()).value
        let a = a.converted(to: UnitDuration.baseUnit()).value
        return Length(value: a * b, unit: UnitLength.baseUnit())
    }
    
    /// Support ab = b * a where `a` is duration, `b` is speed, and `ab` is length
    public static func * (b: Speed, a: Duration) -> Length { return a * b }
    
    /// Support b = ab / a where `a` is duration, `b` is speed, and `ab` is length
    public static func / (ab: Length, a: Duration) -> Speed {
        let a = a.converted(to: UnitDuration.baseUnit()).value
        let ab = ab.converted(to: UnitLength.baseUnit()).value
        return Speed(value: ab / a, unit: UnitSpeed.baseUnit())
    }
    
    /// Support a = ab / b where `a` is duration, `b` is speed, and `ab` is length
    public static func / (ab: Length, b: Speed) -> Duration {
        let b = b.converted(to: UnitSpeed.baseUnit()).value
        let ab = ab.converted(to: UnitLength.baseUnit()).value
        return Duration(value: ab / b, unit: UnitDuration.baseUnit())
    }
}
    
extension Measurement {
    
    ///////////////////////////////////////////////////////////////////////////////
    // MARK: - Handle acceleration-related expressions
    
    /// Support ab = a * b where `a` is acceleration, `b` is duration, and `ab` is speed
    public static func * (a: Acceleration, b: Duration) -> Speed {
        let b = b.converted(to:UnitDuration.baseUnit()).value
        let a = a.converted(to: UnitAcceleration.baseUnit()).value
        return Speed(value: a * b, unit: UnitSpeed.baseUnit())
    }
    
    /// Support ab = b * a where `a` is acceleration, `b` is duration, and `ab` is speed
    public static func * (b: Duration, a: Acceleration) -> Speed { return a * b }
    
    /// Support b = ab / a where `a` is acceleration, `b` is duration, and `ab` is speed
    public static func / (ab: Speed, a: Acceleration) -> Duration {
        let a = a.converted(to: UnitAcceleration.baseUnit()).value
        let ab = ab.converted(to: UnitSpeed.baseUnit()).value
        return Duration(value: ab / a, unit: UnitDuration.baseUnit())
    }
    
    /// Support a = ab / b where `a` is acceleration, `b` is duration, and `ab` is speed
    public static func / (ab: Speed, b: Duration) -> Acceleration {
        let b = b.converted(to: UnitDuration.baseUnit()).value
        let ab = ab.converted(to: UnitSpeed.baseUnit()).value
        return Acceleration(value: ab / b, unit: UnitAcceleration.baseUnit())
    }
    
}

extension Measurement {
    
    ///////////////////////////////////////////////////////////////////////////////
    // MARK: - Handle area-related expressions
    
    /// Support ab = a * b where `a` and `b` are lengths, and `ab` is area
    public static func * (a: Length, b: Length) -> Area {
        let b = b.converted(to: UnitLength.baseUnit()).value
        let a = a.converted(to: UnitLength.baseUnit()).value
        return Area(value: a * b, unit: UnitArea.baseUnit())
    }
    
    /// Support b = ab / a where `a` and `b` are lengths, and `ab` is area
    public static func / (ab: Area, a: Length) -> Length {
        let ab = ab.converted(to: UnitArea.baseUnit()).value
        let a = a.converted(to: UnitLength.baseUnit()).value
        return Length(value: ab / a, unit: UnitLength.baseUnit())
    }
    
    ///////////////////////////////////////////////////////////////////////////////
    // MARK: - Handle volume-related expressions
    
    /// Support ab = a * b where `a` is area and `b` is length, and `ab` is volume
    public static func * (a: Area, b: Length) -> Volume {
        let b = b.converted(to: UnitLength.baseUnit()).value
        let a = a.converted(to: UnitArea.baseUnit()).value
        return Volume(value: a * b, unit: UnitVolume.cubicMeters)
    }
    
    /// Support ab = b * a where `a` is area and `b` is length, and `ab` is volume
    public static func * (b: Length, a: Area) -> Volume { return a * b }
    
    /// Support a = ab / b where `a` is area and `b` is length, and `ab` is volume
    public static func / (ab: Volume, b: Length) -> Area {
        let ab = ab.converted(to: UnitVolume.cubicMeters).value
        let b = b.converted(to: UnitLength.baseUnit()).value
        return Area(value: ab / b, unit: UnitArea.baseUnit())
    }
    
    /// Support b = ab / a where `a` is area and `b` is length, and `ab` is volume
    public static func / (ab: Volume, a: Area) -> Length {
        let ab = ab.converted(to: UnitVolume.cubicMeters).value
        let a = a.converted(to: UnitArea.baseUnit()).value
        return Length(value: ab / a, unit: UnitLength.baseUnit())
    }
    
}

extension Measurement {
    
    ///////////////////////////////////////////////////////////////////////////////
    // MARK: - Handle Ohms law
    
    /// Support ab = a * b where `a` is current and `b` is resistance, and `ab` is voltage
    public static func * (a: Current, b: Resistance) -> Voltage {
        let b = b.converted(to: UnitElectricResistance.baseUnit()).value
        let a = a.converted(to: UnitElectricCurrent.baseUnit()).value
        return Voltage(value: a * b, unit: UnitElectricPotentialDifference.baseUnit())
    }
    
    /// Support ab = b * a where `a` is current and `b` is resistance, and `ab` is voltage
    public static func * (b: Resistance, a: Current) -> Voltage { return a * b }
    
    /// Support a = ab / b where `a` is current and `b` is resistance, and `ab` is voltage
    public static func / (ab: Voltage, b: Resistance) -> Current {
        let ab = ab.converted(to: UnitElectricPotentialDifference.baseUnit()).value
        let b = b.converted(to: UnitElectricResistance.baseUnit()).value
        return Current(value: ab / b, unit: UnitElectricCurrent.baseUnit())
    }
    
    /// Support b = ab / a where `a` is current and `b` is resistance, and `ab` is voltage
    public static func / (ab: Voltage, a: Current) -> Resistance {
        let ab = ab.converted(to: UnitElectricPotentialDifference.baseUnit()).value
        let a = a.converted(to: UnitElectricCurrent.baseUnit()).value
        return Resistance(value: ab / a, unit: UnitElectricResistance.baseUnit())
    }
    
    ///////////////////////////////////////////////////////////////////////////////
    // MARK: - Handle Power law
    
    /// Support ab = a * b where `a` is current and `b` is voltage, and `ab` is power
    public static func * (a: Current, b: Voltage) -> Power {
        let b = b.converted(to: UnitElectricPotentialDifference.baseUnit()).value
        let a = a.converted(to: UnitElectricCurrent.baseUnit()).value
        return Power(value: a * b, unit: UnitPower.baseUnit())
    }
    
    /// Support ab = b * a where `a` is current and `b` is voltage, and `ab` is power
    public static func * (b: Voltage, a: Current) -> Power { return a * b }
    
    /// Support a = ab / b where `a` is current and `b` is voltage, and `ab` is power
    public static func / (ab: Power, b: Voltage) -> Current {
        let ab = ab.converted(to: UnitPower.baseUnit()).value
        let b = b.converted(to: UnitElectricPotentialDifference.baseUnit()).value
        return Current(value: ab / b, unit: UnitElectricCurrent.baseUnit())
    }
    
    /// Support b = ab / a where `a` is current and `b` is voltage, and `ab` is power
    public static func / (ab: Power, a: Current) -> Voltage {
        let ab = ab.converted(to: UnitPower.baseUnit()).value
        let a = a.converted(to: UnitElectricCurrent.baseUnit()).value
        return Voltage(value: ab / a, unit: UnitElectricPotentialDifference.baseUnit())
    }
    
}

extension Measurement {
    
    ///////////////////////////////////////////////////////////////////////////////
    // MARK: - Handle energy-related expressions
    
    /// Support ab = a * b where `a` is power and `b` is duration, and `ab` is energy
    public static func * (a: Power, b: Duration) -> Energy {
        let b = b.converted(to: UnitDuration.baseUnit()).value
        let a = a.converted(to: UnitPower.baseUnit()).value
        return Energy(value: a * b, unit: UnitEnergy.baseUnit())
    }
    
    /// Support ab = b * a where `a` is power and `b` is duration, and `ab` is energy
    public static func * (b: Duration, a: Power) -> Energy { return a * b }
    
    /// Support a = ab / b where `a` is power and `b` is duration, and `ab` is energy
    public static func / (ab: Energy, b: Duration) -> Power {
        let ab = ab.converted(to: UnitEnergy.baseUnit()).value
        let b = b.converted(to: UnitDuration.baseUnit()).value
        return Power(value: ab / b, unit: UnitPower.baseUnit())
    }
    
    /// Support b = ab / a where `a` is power and `b` is duration, and `ab` is energy
    public static func / (ab: Energy, a: Power) -> Duration {
        let ab = ab.converted(to: UnitEnergy.baseUnit()).value
        let a = a.converted(to: UnitPower.baseUnit()).value
        return Duration(value: ab / a, unit: UnitDuration.baseUnit())
    }
    
    /// Support ab = a * b where `a` is charge and `b` is voltage, and `ab` is energy
    public static func * (a: Charge, b: Voltage) -> Energy {
        let b = b.converted(to: UnitElectricPotentialDifference.baseUnit()).value
        let a = a.converted(to: UnitElectricCharge.baseUnit()).value
        return Energy(value: a * b, unit: UnitEnergy.baseUnit())
    }
    
    /// Support ab = b * a where `a` is charge and `b` is voltage, and `ab` is energy
    public static func * (b: Voltage, a: Charge) -> Energy { return a * b }
    
    /// Support a = ab / b where `a` is charge and `b` is voltage, and `ab` is energy
    public static func / (ab: Energy, b: Voltage) -> Charge {
        let ab = ab.converted(to: UnitEnergy.baseUnit()).value
        let b = b.converted(to: UnitElectricPotentialDifference.baseUnit()).value
        return Charge(value: ab / b, unit: UnitElectricCharge.baseUnit())
    }
    
    /// Support b = ab / a where `a` is charge and `b` is voltage, and `ab` is energy
    public static func / (ab: Energy, a: Charge) -> Voltage {
        let ab = ab.converted(to: UnitEnergy.baseUnit()).value
        let a = a.converted(to: UnitElectricCharge.baseUnit()).value
        return Voltage(value: ab / a, unit: UnitElectricPotentialDifference.baseUnit())
    }
}

extension Measurement {
    
    ///////////////////////////////////////////////////////////////////////////////
    // MARK: - Handle charge-related expressions
    
    /// Support ab = a * b where `a` is current and `b` is duration, and `ab` is charge
    public static func * (a: Current, b: Duration) -> Charge {
        let b = b.converted(to: UnitDuration.hours).value
        let a = a.converted(to: UnitElectricCurrent.baseUnit()).value
        return Charge(value: a * b, unit: UnitElectricCharge.ampereHours)
    }
    
    /// Support ab = b * a where `a` is current and `b` is duration, and `ab` is charge
    public static func * (b: Duration, a: Current) -> Charge { return a * b }
    
    /// Support a = ab / b where `a` is current and `b` is duration, and `ab` is charge
    public static func / (ab: Charge, b: Duration) -> Current {
        let ab = ab.converted(to: UnitElectricCharge.ampereHours).value
        let b = b.converted(to: UnitDuration.hours).value
        return Current(value: ab / b, unit: UnitElectricCurrent.baseUnit())
    }
    
    /// Support b = ab / a where `a` is current and `b` is duration, and `ab` is charge
    public static func / (ab: Charge, a: Current) -> Duration {
        let ab = ab.converted(to: UnitElectricCharge.ampereHours).value
        let a = a.converted(to: UnitElectricCurrent.baseUnit()).value
        return Duration(value: ab / a, unit: UnitDuration.hours)
    }
}

extension UnitDuration {
    
    static let days = UnitDuration(symbol: "d", converter: UnitConverterLinear(coefficient: 24*60*60))
    static let weeks = UnitDuration(symbol: "w", converter: UnitConverterLinear(coefficient: 7*24*60*60))
    
}
