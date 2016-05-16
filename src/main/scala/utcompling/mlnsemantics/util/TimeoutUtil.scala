package utcompling.mlnsemantics.util

//import scala.actors.Futures._
//import scala.actors.threadpool.TimeoutException

object TimeoutUtil {
  /*def runWithTimeout[T](timeoutMs: Long)(f: => T) : Option[T] = {
    awaitAll(timeoutMs, future(f)).head.asInstanceOf[Option[T]]
  }

  def runWithTimeout[T](timeoutMs: Long, default: T)(f: => T) : T = {
    runWithTimeout(timeoutMs)(f).getOrElse(default)
  }
  */
  def runWithTimeoutJava(timeoutMs: Long)(f: => Any) : Int = 
  {
		val thread = new Thread 
		{
			override def run () = 
			{
				f
			}
		}

		thread.start()
		thread.join(timeoutMs)
		if (thread.isAlive)
		{
			thread.stop();
			return -1;
		}
		else return 0
  }

}