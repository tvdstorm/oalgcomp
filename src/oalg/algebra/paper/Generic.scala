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

object Generic extends App {
  import Exp._
  import Merge._
	
  trait Algebra[F[_]] {
    // Basic combinators
    def merge[A,B](mix : Lifter[A,B], a1 : F[A], a2 : F[B]) : F[A with B] 
    def empty() : F[Any] 
  
    // Derived combinator(s)
    def decorate[A](parent : F[A], action : A => A) : F[A] =
      merge[A,Any](new LiftDecorate(action),parent,empty)
  }
  
  
  object ExpComb extends Algebra[ExpAlg] {
    def merge[A,B](mix : Lifter[A,B], a1 : ExpAlg[A], a2 : ExpAlg[B]) = 
      new ExpMerge[A,B]() {
        val lifter = mix
        val alg1  = a1
        val alg2  = a2
      }
    
    def empty = ExpEmpty
  }
  
  def test() = {
    // client code 
    import ExpComb._
    val o = exp(merge(LiftEP,decorate(ExpEval,TraceEval.action),ExpPrint))
    println("Eval: " + o.eval() + "\n PP: " + o.print())
  }
  
  override def main(args: Array[String]) = {
    test
  }
}