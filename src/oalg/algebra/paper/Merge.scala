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


object Merge {
  import Exp._
  
  trait Lifter[A,B] {
    def lift(x : A, y : B ) : A with B
  }
  
  class MkLifter[A,B](f : (A,B) => A with B) extends Lifter[A,B] {
    def lift(x : A, y : B) : A with B = f(x,y)
  }
  
  trait ExpMerge[A,B] extends ExpAlg[A with B] {
    val lifter : Lifter[A,B]
    val alg1  : ExpAlg[A]
    val alg2  : ExpAlg[B]
  
    def Lit(x : Int) : A with B =
      lifter.lift(alg1.Lit(x),alg2.Lit(x))
    
    def Add(e1 : A with B, e2 : A with B) : A with B =
      lifter.lift(alg1.Add(e1, e2),alg2.Add(e1, e2))
  }
  
  // client code
  def exp[Exp](f : ExpAlg[Exp]) : Exp =
    f.Add(f.Lit(5), f.Add(f.Lit(6),f.Lit(6)))
  
    
  // Need to redefine ExpEval and ExpPrint
  trait ExpEval extends ExpAlg[IEval] {
    def Lit(x : Int) : IEval = new IEval {
  	  def eval() = x
    }
	  
    def Add(e1 : IEval, e2 : IEval) : IEval = new IEval {
	  def eval() = e1.eval() + e2.eval()
	}
  }
  
  object LiftEP extends Lifter[IEval,IPrint] {
    def lift(x : IEval, y : IPrint) = new IEval with IPrint {
      def print() = y.print()
      def eval() = x.eval()
    }
  }
  
  object ExpPrintEval extends ExpMerge[IEval,IPrint] {
    val alg1 = ExpEval 
    val alg2 = ExpPrint
    val lifter = LiftEP
  } 
    
  def test2() = {
    val o = exp(ExpPrintEval)
    
    println("Eval: " + o.eval() + "\nPrint: " + o.print())
  }

  trait ExpEmpty extends ExpAlg[Any] {
    def Lit(x : Int) : Any = new Object()
    def Add(e1 : Any, e2 : Any) : Any = new Object ()
  }
  
  object ExpEmpty extends ExpEmpty

  class LiftDecorate[A](action : A => A) extends Lifter[A,Any] {
    def lift(x : A, y : Any) = action(x)
  }

  trait ExpDecorate[A] extends ExpMerge[A,Any] {
    val alg2 = ExpEmpty
    val lifter = new LiftDecorate(action)
    def action(x : A) : A
  }
     
  object TraceEval extends ExpDecorate[IEval] {
    val alg1 = ExpEval
    
    def action(o : IEval) = new IEval() {
      def eval() = {
        println("Entering eval()!")
        o.eval()
      }
    }
  }
     
  def test3() = {
    val o = exp(TraceEval)
    
    println("Eval: " + o.eval())
  }
  
}