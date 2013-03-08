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
package oalg.algebra.paper

object Exp {

	trait ExpAlg[Exp] { 
	  def Lit(x : Int) : Exp
	  def Add(e1 : Exp, e2 : Exp) : Exp
	}
	
	trait IEval {
	  def eval() : Int
	}
	
	trait ExpEval extends ExpAlg[IEval] {  
	  def Lit(x : Int) : IEval = new IEval {
	    def eval() : Int = x
	  }
	  
	  def Add(e1 : IEval, e2 : IEval) : IEval = new IEval {
	    def eval() : Int = e1.eval() + e2.eval() 
	  }
	}
	
	object ExpEval extends ExpEval
	
	
	trait IPrint {
	  def print() : String
	}
	
	trait ExpPrint extends ExpAlg[IPrint] {  
	  def Lit(x : Int) : IPrint = new IPrint {
	    def print() : String = x.toString()
	  }
	  
	  def Add(e1 : IPrint, e2 : IPrint) : IPrint = new IPrint {
	    def print() : String = e1.print() + " + " + e2.print() 
	  }
	}
	
	object ExpPrint extends ExpPrint
	
	object OATesting {
	  
	  def exp[Exp](f : ExpAlg[Exp]) : Exp =
	    f.Add(f.Lit(5), f.Add(f.Lit(6),f.Lit(6)))
	    
	  val test1 = {
	    val o1 : IEval = exp(ExpEval)
	    val o2 : IPrint = exp(ExpPrint)
	    
	    println("Eval: " + o1.eval() + "\nPrint: " + o2.print())
	  }
	  
	}
	
	
	// Combinators using pairs
	
	trait ExpMergePair[A,B] extends ExpAlg[(A,B)] {
	  val alg1 : ExpAlg[A]
	  val alg2 : ExpAlg[B]
	  
	  def Lit(x : Int) : (A,B) = 
	    (alg1.Lit(x), alg2.Lit(x))
	    
	  def Add(e1 : (A,B), e2 : (A,B)) : (A,B) =
	    (alg1.Add(e1._1,e2._1), alg2.Add(e1._2,e2._2))
	}
	
	object OACTesting {
	   
	  def exp[Exp](f : ExpAlg[Exp]) : Exp =
	    f.Add(f.Lit(5), f.Add(f.Lit(6),f.Lit(6)))
	
	  object ExpPrintEval extends ExpMergePair[IPrint,IEval] {
	    val alg1 = ExpPrint
	    val alg2 = ExpEval
	  } 
	    
	  val test2 = {
	    val o : (IPrint,IEval) = exp(ExpPrintEval)
	    println("Eval: " + o._2.eval() + "\nPrint: " + o._1.print())
	  }
	}
}