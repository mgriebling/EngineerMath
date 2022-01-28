/*-------------------------------------------------------------------------
    String+Char.swift -- String/Character extensions
    Compiler Generator Coco/R
    Copyright (c) by Michael Griebling

    This program is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 2, or (at your option) any
    later version.

    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
    or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

    As an exception, it is allowed to write an extension of Coco/R that is
    used as a plugin in non-free software.

    If not otherwise stated, any source code generated by Coco/R (other than
    Coco/R itself) does not fall under the GNU General Public License.
-------------------------------------------------------------------------*/

import Foundation

public extension String {
	
	// Extensions to make it easier to work with C-style strings
	
//    subscript (n: Int) -> Character {
//        get {
//            let s = self.index(self.startIndex, offsetBy: n)
//            if s < self.endIndex {
//                return self[s]
//            }
//            return "\0"
//        }
//        set {
//            let s = self.index(self.startIndex, offsetBy: n)
//            if s < self.endIndex {
//                let str = self[index(after: s)...]
//                self = self[...s] + "\(newValue)" + str
//            }
//        }
//	}
//	
//    func count() -> Int { return self.count }
//	
//    func stringByTrimmingTrailingCharactersInSet (_ characterSet: CharacterSet) -> String {
//		if let rangeOfLastWantedCharacter = self.rangeOfCharacter(from: characterSet.inverted, options:.backwards) {
//			return String(self[...rangeOfLastWantedCharacter.upperBound])
//		}
//		return ""
//	}
//    
//    func substring (_ from: Int, _ length: Int) -> String {
//        let str = self as NSString
//        return str.substring(with: NSMakeRange(from, length))
//    }
//    
//    func contains (_ s: String) -> Bool {
//        let str = self as NSString
//        return str.contains(s)
//    }
//    
//    func trim() -> String {
//        return self.trimmingCharacters(in: CharacterSet.whitespaces)
//    }
	
}

public extension Character {

//    var unicodeValue : Int { return Int(unicodeScalar.value) }
//    var unicodeScalar : UnicodeScalar { return String(self).unicodeScalars.first ?? "\0" }
//    
//    func isLetter() -> Bool { return CharacterSet.letters.contains(unicodeScalar) }
//    func isAscii() -> Bool { return unicodeScalar.isASCII }
//    
//    func isAlphanumeric() -> Bool { return CharacterSet.alphanumerics.contains(unicodeScalar) }
//    
//    var lowercase : Character {
//        let s = String(self).lowercased(with: Locale.current)
//        return s.first ?? self
//    }
//	
//	init(_ int: Int) { self = Character(UnicodeScalar(int)!) }
//	
//    func add (_ n: Int) -> Character { return Character(self.unicodeValue + n) }
//
//    static func == (l: Int, r: Character) -> Bool { return l == r.unicodeValue }
//    static func == (l: Character, r: Int) -> Bool { return r == l }
//    static func != (l: Int, r: Character) -> Bool { return l != r.unicodeValue }
//    static func != (l: Character, r: Int) -> Bool { return r != l }
//    static func + (c: Character, inc: Int) -> Character { return c.add(inc) }
//    static func - (c: Character, inc: Int) -> Character { return c.add(-inc) }
//    static func - (c: Character, inc: Character) -> Int { return c.add(-inc.unicodeValue).unicodeValue }
//    static func += (c: inout Character, inc: Int) { c = c + inc }
//    static func -= (c: inout Character, inc: Int) { c = c - inc }
    
}
