package org.romeo.loveletter.persistence

import scala.collection.mutable

/**
  * User: tylerromeo
  * Date: 2/8/16
  * Time: 2:59 PM
  *
  */
trait Datastore[A] {
  def exists(key: String): Boolean
  def get(key: String): Option[A]
  def put(key: String, value: A): Option[A]
  def remove(key: String): Unit
}

class MemoryDataStore[A] extends Datastore[A] {

  var map: scala.collection.mutable.Map[String, A] = new mutable.HashMap[String, A]()

  override def exists(key: String): Boolean = map.contains(key)

  override def get(key: String): Option[A] = map.get(key)

  override def put(key: String, value: A): Option[A] = map.put(key, value)

  override def remove(key: String): Unit = map.remove(key)
}
