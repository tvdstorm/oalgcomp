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

import scala.collection.mutable.HashMap
import scala.reflect._

object Self {
  import Exp.IEval
  import Exp.IPrint
  
  trait GExpAlg[In,Out] {
	  def Lit(x : Int) : Out
	  def Add(e1 : In, e2 : In) : Out
  }
  
  type ExpAlg[E] = GExpAlg[E,E]
  
  type Open[S <: E, E] = (=> S) => E
  type OExpAlg[S <: E, E] = GExpAlg[S, Open[S,E]]
  
  trait ExpEval extends ExpAlg[IEval] {  
    def Lit(x : Int) : IEval = new IEval {
      def eval() : Int = x
    }
  
    def Add(e1 : IEval, e2 : IEval) : IEval = new IEval {
      def eval() : Int = e1.eval() + e2.eval() 
    }
  }

  object ExpEval extends ExpEval
  
  // Use the core algebra types and combinators
  import oalg.algebra.core.Algebras._
  
  class LiftEP[S <: IEval with IPrint] extends Lifter[IEval,IPrint,S] {
    def lift(x : IEval, y : IPrint) = self => new IEval with IPrint {
      def print() = y.print()
      def eval() = x.eval()
    }
  }
  
  object TraceEval extends ExpAlg[IEval] {
    def action(o : IEval) = new IEval() {
      def eval() = {
        println("Entering eval()!")
        o.eval()
      }
    }
    
    def Lit(x : Int) = throw new Exception()
    def Add(e1 : IEval, e2 : IEval) = throw new Exception()
  }
  
  trait ExpPrint2[S <: IEval with IPrint] extends OExpAlg[S, IPrint] { 
    def Lit(x : Int) = self => new IPrint() {
      def print() = x.toString()
    }
  
    def Add(e1 : S, e2 : S) = self => new IPrint() {
      def print() = e1.print() + " + " + e2.print() + " = " + self.eval()
    }
  }
  
  // Closing
  trait CloseAlg[E] extends ExpAlg[E] {
    val alg : OExpAlg[E,E]
	  
    def Lit(x : Int) : E = fix(alg.Lit(x))
    def Add(e1 : E, e2 : E) : E = fix(alg.Add(e1,e2))
  }
	
  def closeAlg[E](a : OExpAlg[E,E]) : ExpAlg[E] = new CloseAlg[E] {
    val alg = a
  }
  
  def fix[A](f : Open[A,A]) : A = {lazy val s : A = f(s); s}
  
  // requires a closed (object algebra) component
  def exp[A](f : ExpAlg[A]) = {
    import f._
	  
    Add(Lit(13),Lit(5))
  }
	
  
  // FAMILY Self-references
  
  // Algebras with a family self-reference
  trait SelfAlg[Self <: Exp, Exp] {
    val fself : ExpAlg[Self]
  }
  
  trait SelfExpAlg[Self <: Exp, Exp] extends GExpAlg[Self,Open[Self,Exp]] with SelfAlg[Self,Exp]
  
  trait ExpPrint3[Self <: IEval with IPrint] extends SelfExpAlg[Self,IPrint]{
	def Lit(x : Int) = self => new IPrint() {
        def print() = x.toString()
    }
	
	def Add(e1 : Self, e2 : Self) = self => new IPrint() {
	  def print() = { 
	    val plus54 = fself.Add(fself.Lit(5), fself.Lit(4)); // virtual constructors
	    e1.print() + " + " + e2.print() + " = " + self.eval() + "   and " + "5 + 4 = " + plus54.eval(); // self-reference
	  }
	}
  }
  
  def ExpPrint3[S <: IEval with IPrint] : OpenExpAlg[S,IPrint] = s => new ExpPrint3[S] {
    lazy val fself = s
  }
  
  trait ExpEval2[Self <: IEval] extends SelfExpAlg[Self,IEval] {  
    def Lit(x : Int) = self => new IEval {
      def eval() : Int = x
    }
  
    def Add(e1 : Self, e2 : Self) = self => new IEval {
      def eval() : Int = e1.eval() + e2.eval() 
    }
  }
  
  def expEval[S <: IEval] : OpenExpAlg[S,IEval] = s => new ExpEval2[S] {
    lazy val fself = s
  }
  
  type OpenExpAlg[S <: A, A] = (=> ExpAlg[S]) => GExpAlg[S, Open[S,A]]
  
  def compose[A,B, C](f : B => C, g : A => B) : A => C = x => f(g(x))
  
  def close[S](f : OpenExpAlg[S,S]) : ExpAlg[S] = 
    fix[ExpAlg[S]](compose(closeAlg,f))  
  
   
  // GENERIC
  type OFAlg[F[_,_],S <: E, E] = F[S, Open[S,E]]
  type FAlg[F[_,_],E] = F[E,E]
  
  trait SelfFAlg[F[_,_],Self <: Exp, Exp] {
    val fself : F[Self, Self]
  }
  
  class LiftDecorate[S <: A, A](action : A => A) extends Lifter[A,Any,S] {
    def lift(x : A, y : Any) = self => action(x)
  }

  class MkLifter[A,B, S <: A with B](f : (A,B) => A with B) extends Lifter[A,B,S] {
    def lift(x : A, y : B) : Open[S,A with B] = self => f(x,y)
  }
  
  object ExpComb extends AlgebraDefault[GExpAlg]
  
  def test3() = {
    import ExpComb._
    
    val o = exp(fclose(merge[IEval,IPrint,IEval with IPrint](new LiftEP,
        decorate(expEval,TraceEval.action),ExpPrint3)))
    println("Eval: " + o.eval() + "\nPrint: " + o.print())
  }
  
  type ExpAlgOpen[S <: T, T] = (=> GExpAlg[S,S]) => GExpAlg[S,Open[S,T]]
}