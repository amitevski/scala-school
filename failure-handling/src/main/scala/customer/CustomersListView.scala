package customer

import org.scalactic.Good

import scala.util.{Success, Try}

class CustomersListView(customerService: CustomerService) {

  /**
   * Use [[customerService.customerName(id)]] for implementing a Seq of existing customer names for the given customer ids.
   * I.e. remove things for that the Repo throws it's customer not found exception.
   *
   * You should try to use the scala language try/catch here (not the scala library Try class) for handling the exceptions of the underlying repo method.
   *
   * Do you feel the burden that is put on the client when using Exceptions for "business return values"?
   * Code using exceptions and try/catch language construct usually feels less functional.
   */
  def listExistingCustomerNamesLangTryBased(ids: Seq[Int]): Seq[String] = {
    ids.flatMap {
      customerId =>
        try {
          Some(customerService.customerName(customerId))
        } catch {
          case e: CustomerNotFoundException => None
        }
    }
  }

  /**
   * Same as above but now use already "Try wrapped" [[customerService.customerNameTry(id)]].
   *
   * Hint: Have a look at collect method in http://www.scala-lang.org/api/current/#scala.collection.Seq in conjunction with pattern matching.
   * Remember with pattern matching you may define a partial function!
   *
   * The scala library Try is usually a good fit to integrate code/libraries that uses exceptions in a functional manner.
   * See http://www.scala-lang.org/api/current/#scala.util.Try for great examples on Try.
   */
  def listExistingCustomerNamesTryBased(ids: Seq[Int]): Seq[String] = {
    ids.map(customerService.customerNameTry).collect {
      case Success(name) => name
    }
  }

  /**
   * Same as above but now use [[customerService.customerNameOption(id)]].
   *
   * Hint: Have a look at map() and flatten() on Seq to solve this problem very concisely. Is there a way to be even more concise?
   */
  def listExistingCustomerNamesOptionBased(ids: Seq[Int]): Seq[String] = ids.flatMap(customerService.customerNameOption)

  /**
   * Same as above but now use [[customerService.customerNameOr(id)]].
   *
   * Hint: Have a look at collect method in http://www.scala-lang.org/api/current/#scala.collection.Seq in conjunction with pattern matching.
   * Remember with pattern matching you may define a partial function!
   */
  def listExistingCustomerNamesScalacticOrBased(ids: Seq[Int]): Seq[String] = {
    ids.map(customerService.customerNameOr).collect {
      case Good(name) => name
    }
  }
}
