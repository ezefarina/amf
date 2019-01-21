package amf.core.remote

import java.util.concurrent.locks.ReentrantReadWriteLock

import amf.core.exception.CyclicReferenceException
import amf.core.model.document.BaseUnit

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object GlobalCounter {
  var v = 0
}

class Cache {

  def isBeingParsed(url: String, father: String): Boolean = synchronized {
    cache.get(url).exists(_._2.contains(father))
  }
  def valuesFor(url: String): List[String] = synchronized { cache.get(url).map(_._2).getOrElse(Nil).toList }

  // seq the uses
  protected val cache: mutable.Map[String, (Future[BaseUnit], Set[String])] = synchronized(mutable.Map())

  def getOrUpdate(url: String)(supplier: () => Future[BaseUnit]): Future[BaseUnit] =
    cache.get(url) match {
      case Some((f, fathers)) =>
        cache.update(url, (f, Set()))
        f
      case None =>
        val futureUnit = supplier()
        update(url, futureUnit, Set())
        futureUnit
    }

  private def searchUses(father: String, url: String, context: Context): Boolean = synchronized {
    cache.get(father) match {
      case Some((_, dependencies)) if dependencies.contains(url) =>
        true
//      case Some((_, dependencies)) if dependencies.exists(s => searchUses(s, url, context)) => throw new CyclicReferenceException(context.history)
      case _ => false
    }
  }

  def getOrUpdateSynchronized(url: String, context: Option[Context])(
      supplier: () => Future[BaseUnit]): Future[Option[BaseUnit]] = synchronized {

//    if(context.exists(c => searchUses(c.current,url, c))) {
//      cache.get(url) match {
//        case Some((f, dependencies)) => cache.put(url, (f, dependencies ++ context.map(_.current)))
//        case _ => //ignore
//      }
//      Future.failed(new CyclicReferenceException(context.get.history))
//    }
//    else
    cache.get(url) match {
      case Some((f, fathers)) =>
        cache.update(url, (f, fathers ++ context.map(_.current).toSet))
        f.map(Some(_))
      case None =>
        val futureUnit = supplier()
        update(url, futureUnit, context.map(_.current).toSet)
        futureUnit.map(Some(_))
    }
  }

  private def update(url: String, value: Future[BaseUnit], father: Set[String]): Unit = synchronized {
    cache.update(url, (value, father))
  }

  protected def size: Int = cache.size
}

object Cache {
  def apply(): Cache = {
    new Cache()
  }
}
