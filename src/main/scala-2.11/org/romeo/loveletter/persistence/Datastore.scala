package org.romeo.loveletter.persistence
import spray.json.JsValue

import scala.collection.mutable

/**
  * User: tylerromeo
  * Date: 2/8/16
  * Time: 2:59 PM
  *
  */
trait Datastore {
  def exists(key: String): Boolean
  def get(key: String): Option[JsValue]
  def put(key: String, value: JsValue): Unit
}

class MemoryDataStore extends Datastore {

  var map: scala.collection.mutable.Map[String, JsValue] = new mutable.HashMap[String, JsValue]()

  override def exists(key: String): Boolean = map.contains(key)

  override def get(key: String): Option[JsValue] = map.get(key)

  override def put(key: String, value: JsValue): Unit = map.put(key, value)
}
