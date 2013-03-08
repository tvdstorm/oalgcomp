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
package oalg.algebra.demo.stacks

import oalg.algebra.core.Algebras._
import scala.reflect._

object Stacks {
  trait Stack {
    def empty(): Unit
    def push(s: Char): Unit
    def push2(s: Char): Unit
    def pop(): Unit
    def top(): Char
  }

  trait Counter {
    def reset(): Unit
    def inc(): Unit
    def dec(): Unit
    def size(): Int
  }

  trait Lock {
    def lock(): Unit
    def unlock(): Unit
    def is_unlocked(): Boolean
  }
  
  trait Bound {
    def check(s: Char): Boolean
  }
  
  trait Undo {
    def save(s: String): Unit
    def undo(): String
  }
  
  trait StackAlg[In,Out] { def stack() : Out }
  
  trait SelfStackAlg[S <: E, E] extends 
     StackAlg[S,Open[S, E]] with SelfFAlg[StackAlg,S,E]  
  
  type OpenStackAlg[S <: E, E] = (=> StackAlg[S,S]) => StackAlg[S,Open[S, E]]
  
  object StackComb extends AlgebraDefault[StackAlg] 

  trait CounterAlg[In,Out] { def counter() : Out }
  
  trait SelfCounterAlg[S <: E, E] extends 
     CounterAlg[S,Open[S, E]] with SelfFAlg[CounterAlg,S,E]  
  
  type OpenCounterAlg[S <: E, E] = (=> CounterAlg[S,S]) => CounterAlg[S,Open[S, E]]
  
  object CounterComb extends AlgebraDefault[CounterAlg]
  
  trait SF[S <: Stack] extends SelfStackAlg[S, Stack] {
    def stack() = self => new Stack() {
      var s : String = new String()
      def empty() {s = ""}
      def push(a : Char) {s = a.toString().concat(s) }
      def pop() {s = s.substring(1)}
      def top() = s.charAt(0)
      def push2(a : Char) {self.push(a); self.push(a)} // NB: self, not this
    }
  }
  
  def sf[S <: Stack] : OpenStackAlg[S,Stack] = 
    s => new SF[S] { lazy val fself = s }
    
  class CounterCode extends Counter {
    var i : Int = 0
    def reset() {i = 0}
    def inc() {i = i + 1}
    def dec() {i = i - 1}
    def size() = i
  }  
    
  trait CF[S <: Counter] extends SelfStackAlg[S,Counter] {
    def stack() = self => new CounterCode()
  }
  
  def cf[S <: Counter] : OpenStackAlg[S,Counter] = s => new CF[S] { lazy val fself = s }
  
  trait CFC[S <: Counter] extends SelfCounterAlg[S, Counter] {
    def counter() = self => new CounterCode()
  }
  
  def cfc[S <: Counter] : OpenCounterAlg[S, Counter] = s => new CFC[S] { lazy val fself = s }
  
  class LockCode extends Lock {
    var l : Boolean = true
    def lock() { l = false}
    def unlock() {l = true}
    def is_unlocked() : Boolean = l
  }
  
  trait LF[S <: Lock] extends SelfStackAlg[S,Lock] {
    def stack() = self => new LockCode()
  }
  
  def lf[S <: Lock] : OpenStackAlg[S,Lock] = s => new LF[S] { lazy val fself = s }
  
  trait LFC[S <: Lock] extends SelfCounterAlg[S, Lock] {
    def counter() = self => new LockCode()
  }
  
  def lfc[S <: Lock] : OpenCounterAlg[S, Lock] = s => new LFC[S] { lazy val fself = s }
  
  trait BF[S <: Bound] extends SelfStackAlg[S,Bound] {
    def stack() = self => new Bound {
      def check(s: Char) = Character.isLetter(s) 
    }
  }
  
  def bf[S <: Bound] : OpenStackAlg[S,Bound] = s => new BF[S] {
    lazy val fself = s
  }
  
  // Unused as of yet...
  trait UF[S <: Undo] extends SelfStackAlg[S,Undo] {
    def stack() = self => new Undo {
      var state : scala.collection.mutable.Stack[String] = new scala.collection.mutable.Stack[String]
      def save(s: String) { state.push(s) }
      def undo() = state.pop
    }
  }
  
  def uf[S <: Undo] : OpenStackAlg[S,Undo] = s => new UF[S] {
    lazy val fself = s
  }
  
}