package discats.testing

/** Raised when a [[TestRestClient]] method is called but no stub was configured for it.
  *
  * @param method  The unqualified name of the method that was invoked.
  */
final case class NotStubbedException(method: String)
    extends Exception(
      s"TestRestClient.$method was called but no stub was configured. " +
        s"Call stub${method.capitalize}(...) before exercising code that invokes this method."
    )
