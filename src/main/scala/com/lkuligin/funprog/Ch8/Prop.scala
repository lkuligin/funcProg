package com.lkuligin.funprog.Ch8

trait Prop {
  def check: Boolean

  /**
    * Implement && method of Prop
    */
  def &&(p: Prop): Prop = new Prop {def check = this.check && p.check }
}
