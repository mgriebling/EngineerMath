//
//  MatrixVector.swift
//  TestEngineerMath
//
//  Created by Mike Griebling on 17 May 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Accelerate

class Vector : CustomStringConvertible {
	
	var vector: [Double]
	
	init (size: Int) {
		vector = [Double](repeating: 0, count: size)
	}
	
	init (_ v: [Double]) {
		vector = v
	}
	
	var count: Int {
		return vector.count
	}
	
	var description: String {
		return vector.description
	}
	
	subscript (i: Int) -> Double {
		get {
			if i < vector.count {
				return vector[i]
			}
			return 0
		}
		set (new) {
			if i < vector.count {
				vector[i] = new
			}
		}
	}
	
	func mul (_ v: Vector) -> Vector {
		return Vector(Vector.mul(vector, w: v.vector))
	}
	
	func square () -> Vector {
		return Vector(Vector.square(vector))
	}
	
	func sum () -> Double {
		return Vector.sum(vector)
	}
	
	func sumSquared () -> Double {
		return Vector.sumSquared(vector)
	}
	
	static func add (_ v: [Double], w: Double) -> [Double] {
		var result = [Double](repeating: 0, count: v.count)
		var lw = w
		vDSP_vsaddD(v, 1, &lw, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func mul (_ v: [Double], w: Double) -> [Double] {
		var result = [Double](repeating: 0, count: v.count)
		var lw = w
		vDSP_vsmulD(v, 1, &lw, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func div (_ v: [Double], w: Double) -> [Double] {
		var result = [Double](repeating: 0, count: v.count)
		var lw = w
		vDSP_vsdivD(v, 1, &lw, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func add (_ v: [Double], w: [Double]) -> [Double] {
		var result = [Double](repeating: 0, count: v.count)
		vDSP_vaddD(v, 1, w, 1, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func neg (_ v: [Double]) -> [Double] {
		var result = [Double](repeating: 0, count: v.count)
		vDSP_vnegD(v, 1, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func abs (_ v: [Double]) -> [Double] {
		var result = [Double](repeating: 0, count: v.count)
		vDSP_vabsD(v, 1, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func square (_ v: [Double]) -> [Double] {
		var result = [Double](repeating: 0, count: v.count)
		vDSP_vssqD(v, 1, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func mul (_ v: [Double], w: [Double]) -> [Double] {
		var result = [Double](repeating: 0, count: v.count)
		vDSP_vmulD(v, 1, w, 1, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func div (_ v: [Double], w: [Double]) -> [Double] {
		var result = [Double](repeating: 0, count: v.count)
		vDSP_vdivD(v, 1, w, 1, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func dot (_ v: [Double], w: [Double]) -> Double {
		var result = 0.0
		vDSP_dotprD(v, 1, w, 1, &result, vDSP_Length(v.count))
		return result
	}
	
	static func sort (_ v: inout [Double], ascending: Bool) {
		vDSP_vsortD(&v, vDSP_Length(v.count), ascending ? 1 : -1)
	}
	
	static func max (_ v: [Double]) -> Double {
		var result = 0.0
		vDSP_maxvD(v, 1, &result, vDSP_Length(v.count))
		return result
	}
	
	static func min (_ v: [Double]) -> Double {
		var result = 0.0
		vDSP_minvD(v, 1, &result, vDSP_Length(v.count))
		return result
	}
	
	static func mean (_ v: [Double]) -> Double {
		var result = 0.0
		vDSP_meanvD(v, 1, &result, vDSP_Length(v.count))
		return result
	}
	
	static func rms (_ v: [Double]) -> Double {
		var result = 0.0
		vDSP_rmsqvD(v, 1, &result, vDSP_Length(v.count))
		return result
	}
	
	static func sum (_ v: [Double]) -> Double {
		var result = 0.0
		vDSP_sveD(v, 1, &result, vDSP_Length(v.count))
		return result
	}
	
	static func sumSquared (_ v: [Double]) -> Double {
		var result = 0.0
		vDSP_svesqD(v, 1, &result, vDSP_Length(v.count))
		return result
	}
	
}

class Matrix {
	
	private var matrix: [Double]
	private var irows: Int
	var columns: Int {
		return matrix.count / rows
	}
	var rows: Int {
		return irows
	}
	var count: Int {
		return matrix.count
	}
	
	init (_ a: [Double], numberOfRows: Int) {
		matrix = a
		irows = numberOfRows
	}
	
	subscript (_ col: Int, row: Int) -> Double {
		get {
			let index = row * columns + col
			if index < matrix.count {
				return matrix[index]
			}
			return 0
		}
		set (new) {
			let index = row * columns + col
			if index < matrix.count {
				matrix[index] = new
			}
		}
	}
	
	func add (_ matrix: Matrix) -> Matrix {
		return Matrix(Matrix.add(self.matrix, n: matrix.matrix), numberOfRows: irows)
	}
	
	func getRow (_ row: Int) -> Vector {
		var r = [Double]()
		for i in 0..<columns { r.append(self[i, row]) }
		return Vector(r)
	}
	
	func putRow (_ row: Int, r: Vector) {
		for i in 0..<columns { self[i, row] = r[i] }
	}
	
	func determinant () -> Double {
		return Matrix.determinant(matrix)
	}
	
	func replaceColumn (_ col: Int, withVector v: Vector) -> Matrix? {
		var result = matrix
		if v.count != irows { return nil }
		cblas_ccopy(Int32(v.count), v.vector, 1, &result[col], Int32(irows))
		return Matrix(result, numberOfRows: irows)
	}
	
	static func add (_ m: [Double], n: [Double]) -> [Double] {
		// same as vector add
		var result = [Double](repeating: 0, count: m.count)
		vDSP_vaddD(m, 1, n, 1, &result, 1, vDSP_Length(m.count))
		return result
	}
	
	static func mul (_ m: [Double], mRows: Int, n: [Double], nCols: Int) -> [Double] {
		var result = [Double](repeating: 0, count: mRows*nCols)
		vDSP_mmulD(m, 1, n, 1, &result, 1, vDSP_Length(mRows), vDSP_Length(nCols), vDSP_Length(m.count/mRows))
		return result
	}
	
	static func transpose (_ m: [Double], mRows: Int) -> [Double] {
		var result = [Double](repeating: 0, count: m.count)
		vDSP_mtransD(m, 1, &result, 1, vDSP_Length(m.count/mRows), vDSP_Length(mRows))
		return result
	}
	
	static func invert (_ m: [Double]) -> [Double]? {
		var inMatrix = m
		var M = __CLPK_integer(sqrt(Double(m.count)))
        var N = M
        var LDA = N
		var pivots = [__CLPK_integer](repeating: 0, count: Int(N))
		var workspace = [Double](repeating: 0, count: Int(N))
		var error = __CLPK_integer(0)
        var lWork = N
		
		dgetrf_(&M, &N, &inMatrix, &LDA, &pivots, &error)
		if error != 0 { return nil }
        
        // get optimal workspace
		dgetri_(&N, &inMatrix, &LDA, &pivots, &workspace, &lWork, &error)
		if error != 0 { return nil }
		return inMatrix
	}
	
	static func determinant (_ m: [Double]) -> Double {
		var result = 1.0
		var neg = false
		var error : __CLPK_integer = 0
		var N = __CLPK_integer(sqrt(Double(m.count)))
        var M = N
        var LDA = N
		var pivots = [__CLPK_integer](repeating: 0, count: Int(N))
		var tmp = m
		
		dgetrf_(&M, &N, &tmp, &LDA, &pivots, &error)
		if error != 0 { return 0 } // singular matrix
		
		// Take the product of the diagonal elements
		for i in 0..<Int(N) {
			result *= tmp[i+i*Int(N)]
			if pivots[i] != (i+1) { neg = !neg }
		}
		
		// Return the correct sign
		return neg ? -result : result
	}
	
	static func solveLinear1 (_ m: [Double], n: [Double]) -> [Double]? {
		var a = m
		var b = n
		var TRANS: CChar = 0x4E  // "N"
		var NRHS: __CLPK_integer = 1
		var N = __CLPK_integer(sqrt(Double(m.count)))
        var M = N
        var LDA = N
		var pivots = [__CLPK_integer](repeating: 0, count: Int(N))
		var error : __CLPK_integer = 0
		
		// compute the LU factorization
		dgetrf_(&M, &N, &a, &LDA, &pivots, &error)
		if error != 0 { return nil }
		
		// solve the equations
		dgetrs_(&TRANS, &M, &NRHS, &a, &LDA, &pivots, &b, &N, &error)
		if error != 0 { return nil }
		return b
	}
	
	static func solveLinear2 (_ m: [Double], n: [Double]) -> [Double]? {
		var a = m
		var b = n
		var NRHS: __CLPK_integer = 1
		var N = __CLPK_integer(sqrt(Double(m.count)))
        var M = N
        var LDA = N
		var pivots = [__CLPK_integer](repeating: 0, count: Int(N))
		var error : __CLPK_integer = 0
		
		// compute the LU factorization
//		dgetrf_(&N, &N, &a, &N, &pivots, &error)
//		if error != 0 { return nil }
		
		// solve the equations
		dgesv_(&M, &NRHS, &a, &LDA, &pivots, &b, &N, &error)
		if error != 0 { return nil }
		return b
	}
}

