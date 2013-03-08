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
import java.lang.reflect.Method
import java.lang.reflect.InvocationHandler
import java.lang.reflect.Proxy

object Reflective {
  import Merge._
  import Generic._
  
  def createInstance[A](ih : InvocationHandler)(implicit m : ClassTag[A]) : A = {
      Proxy.newProxyInstance(m.runtimeClass.getClassLoader, Array(m.runtimeClass),ih).asInstanceOf[A]
    }
  
  def delegate[A,B](x : A, y : B)(implicit m : ClassTag[A with B]) : A with B = createInstance(new InvocationHandler() {
      def invoke(proxy : Object, method : Method, args : Array[Object]) : Object = {
	      try {
	        method.invoke(x, args : _*)
	      } catch {
	        case e : IllegalArgumentException => method.invoke(y, args : _*)
	      }
	    }
    })
  
  trait Algebra[F[_]] {
    
    // Basic combinators
    def merge[A,B](lifter : Lifter[A,B], a1 : F[A], a2 : F[B])(implicit m : ClassTag[F[A with B]]) : F[A with B] =  
      createInstance(new InvocationHandler() {
	    def invoke(proxy : Object, method : Method, args : Array[Object]) = {
	      val a = method.invoke(a1,args : _*) 
	      val b = method.invoke(a2,args : _*)
	      lifter.lift(a.asInstanceOf[A],b.asInstanceOf[B]).asInstanceOf[Object]
	    }})
      
    def empty(implicit m : ClassTag[F[Any]]) : F[Any] = 
      createInstance(new InvocationHandler() {
	    def invoke(proxy : Object, method : Method, args : Array[Object]) = new Object()
      })
  
    // Derived combinator(s)
    def delegate[A,B](x : A, y : B)(implicit m : ClassTag[A with B]) : A with B = createInstance(new InvocationHandler() {
      def invoke(proxy : Object, method : Method, args : Array[Object]) : Object = {
	      try {
	        method.invoke(x, args : _*)
	      } catch {
	        case e : IllegalArgumentException => method.invoke(y, args : _*)
	      }
	    }
    })
      
    def combine[A,B](alg1 : F[A], alg2 : F[B])(implicit m1 : ClassTag[F[A with B]], m2 : ClassTag[A with B]) : F[A with B] =
      merge[A,B](new MkLifter(delegate[A,B] _), alg1, alg2)
      
    def decorate[A](parent : F[A], action : A => A)(implicit m1 : ClassTag[F[A]], m2 : ClassTag[F[Any]]) : F[A] =
      merge[A,Any](new LiftDecorate(action),parent,empty)
  }
  
  object Test {
    import Exp._
    object ExpComb extends Algebra[ExpAlg]
  
    def test = {
      import ExpComb._
      val o = exp(merge[IEval, IPrint](LiftEP,ExpEval,ExpPrint))
      println("Eval: " + o.eval() + "\nPrint:" + o.print())
    }
  }
}
