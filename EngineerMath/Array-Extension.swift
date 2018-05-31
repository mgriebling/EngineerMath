//
//  ArrayExtension.swift
//  EngineerMath
//
//  Created by Mike Griebling on 30 May 2018.
//  Copyright © 2018 Solinst Canada. All rights reserved.
//

import Foundation

public struct SafeArray<Element> : MutableCollection {
    
    private var array : [Element]   // Storage of our array elements
    
    public init() { array = [] }
    
    // Needed to conform to a MutableCollection protocol
    public func index(after i: Int) -> Int { return array.index(after: i) }
    public var startIndex: Int { return array.startIndex }
    public var endIndex: Int { return array.endIndex }
    public var count: Int { return array.count }
    mutating public func append(_ x: Element) { array.append(x) }
    
    // Here we modify how subscript works
    public subscript (index: Int) -> Element {
        get { return array[index] }
        set { array[index] = newValue }
    }
}

extension Array {
    
    subscript (safe index: Index) -> Element? {
        return index < self.endIndex ? self[index] : nil
    }
    
}
