/*******************************************************************************
 * Copyright (c) 2012-2013
 * - Bruno C.d.S. Oliveira (oliveira@comp.nus.edu.sg)
 * - Tijs van der Storm (storm@cwi.nl)
 * - Alex Loh (alexloh@cs.utexas.edu)
 * - William R. Cook (wcook@cs.utexas.edu)
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
********************************************************************************/

package oalg.algebra.aspects

object Circular {
  // globals
  var inCircle: Boolean = false
  var change: Boolean = false
}


class Circ[A, B](seed: A, f: B => A) extends (B => A) {
  var computed: Boolean = false;
  var value: A = seed;
  var visited: Boolean = false;
  
  override def apply(v: B): A = {
    if (computed) {
	  return value
	}
	if (!Circular.inCircle) {
	  Circular.inCircle = true
	  visited = true
	  do {
	    Circular.change = false
	    var n = f(v)
	    if (n != value) {
	      Circular.change = true
	    }
	    value = n
	  } while (Circular.change)
	  visited = false
	  computed = true
	  Circular.inCircle = false
	  return value
	}
	else if (!visited) {
	  visited = true
	  var n = f(v)
	  if (n != value) {
	    Circular.change = true
	  }
	  value = n
	  visited = false
	  return value
	}
	else {
	  return value;
	}
  }
}

