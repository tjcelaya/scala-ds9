package co.tjcelaya.sigfig_test.spatialindex

/**
  * Created by tj on 3/10/17.
  */
trait Distanced[T] {
  def distance(thus: T, that: T): Number
}
