//
//  Bag.swift
//  EngineerMath
//
//  Created by Michael Griebling on 3Jun2015.
//  Copyright (c) 2015 Solinst Canada. All rights reserved.
//

import Foundation

public struct Bag<T:Hashable> {
	
	var items = [T: Int]()	// Bag definition as an object and a quantity
	
	public var count : Int {
		var itemCount = 0
		for item in items { itemCount += item.1 }
		return itemCount
	}
	
	public var uniqueCount : Int {
		return items.count
	}
	
	public subscript (_ item: T) -> Int {
		return items[item] ?? 0
	}
	
	public mutating func add (_ item: T) {
		let value = items[item] ?? 0
		items[item] = value+1
	}
	
	public mutating func removeOne (_ item: T) {
		let value = items[item] ?? 0
		if value == 0 { return }
		if value == 1 {
            items.removeValue(forKey: item)
		} else {
			items[item] = value-1
		}
	}
	
	public mutating func removeAll (_ item: T) {
        items.removeValue(forKey: item)
	}
	
	public mutating func removeAllItems () {
		items = [T: Int]()
	}
	
	public func combinedWith (_ bag: Bag<T>) -> Bag<T> {
		var combined = bag
		for item in self {
			let value = (combined.items[item.0] ?? 0) + item.1
			combined.items[item.0] = value
		}
		return combined
	}
	
	public func removeItemsIn (_ bag: Bag<T>) -> Bag<T> {
		var removed = self
		for item in bag {
			if let value = removed.items[item.0] {
				if item.1 < value {
					removed.items[item.0] = value - item.1
				} else {
					removed.removeAll(item.0)
				}
			}
		}
		return removed
	}
	
	public func itemsAlsoIn (_ bag: Bag<T>) -> Bag<T> {
		var intersect = Bag<T>()
		for item in bag {
			if let value = self.items[item.0] {
                intersect.items[item.0] = Swift.min(value, item.1)
			}
		}
		return intersect
	}
	
	public func isEmpty () -> Bool {
		return count == 0
	}
	
	public func isEqual (_ rhs: Bag<T>) -> Bool {
		if self.count != rhs.count { return false }
		if self.uniqueCount != rhs.uniqueCount { return false }
		for item in self {
			if item.1 != rhs[item.0] { return false }
		}
		return true
	}
	
}

extension Bag : Equatable {
    
    static public func == <T>(lhs: Bag<T>, rhs: Bag<T>) -> Bool {
        return lhs.isEqual(rhs)
    }
    
}

extension Bag : Sequence {
    
    public typealias Iterator = DictionaryIterator<T, Int>
    
    public func makeIterator() -> DictionaryIterator<T, Int> {
        return items.makeIterator()
    }
    
}

extension Bag : CustomStringConvertible {
    
    public var description : String {
        var result = ""
        for item in items {
            result += "\(item.0):\(item.1), "
        }
        if result.hasSuffix(", ") { result.removeLast(2) }
        return result
    }
    
}


