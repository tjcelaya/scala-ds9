package co.tjcelaya.ds9.spatialindex

/**
  * Created by tj on 3/10/17.
  */
trait Distanced[T] {
  def distance(thus: T, that: T): Number
}

trait Extrema[T] {
  def minVal: T
  def maxVal: T
}
