package fpinscala.exercises.streamingio
package Proc

sealed trait Proc[I, O]
case class Emit[I, O](
	head: O,
	tail: Proc[I, O] = Halt[I, O]()
) extends Proc[I, O]
case class Await[I, O](
	recv: Option[I] => Proc[I, O]
) extends Proc[I, O]
case class Halt[I, O]() extends Proc[I, O]

def await[I, O](f: Option[I] => Proc[I, O]): Proc[I, O] = Await(f)
def emit[I, O](head: O, tail: Proc[I, O] = Halt[I, O]()): Proc[I, O] = 
	Emit(head, tail)
def liftOne[I, O](f: I => O): Proc[I, O] =
	await {
		case Some(i)	=> emit(f(i))
		case None			=> Halt()
	}
def id[I]: Proc[I, I] = 
	await {
		case Some(i)	=> emit(i, id)
		case None			=> Halt()
	}

extension [I, O](proc: Proc[I, O])
	def apply(src: LazyList[I]): LazyList[O] = proc match
		case Halt()				=> LazyList.empty
		case Await(recv) 	=> src match
			case h #:: t 	=> recv(Some(h))(t)
			case _				=> recv(None)(LazyList.empty)
		case Emit(h, t)		=> h #:: t(src)
		
	def repeat: Proc[I, O] = 
		def go(p: Proc[I, O]): Proc[I, O] = p match
			case Halt() 		=> go(proc)
			case Await(recv)	=> Await {
				case None => recv(None)
				case i		=> go(recv(i))
			}
			case Emit(h, t) => Emit(h, go(t))
		go(proc)

def lift[I, O](f: I => O): Proc[I, O] = 
	liftOne(f).repeat
def filter[I](p: I => Boolean): Proc[I, I] = 
	Await[I, I]{
		case Some(i) if p(i) => Emit(i)
		case _							 => Halt()
	}.repeat
def take[I](n: Int): Proc[I, I] =
	if n <= 0 then Halt()
	else await {
		case Some(i)	=> emit(i, take(n - 1))
		case None			=> Halt()
	}
def drop[I](n: Int): Proc[I, I] = 
	if n <= 0 then id
	else await {
		case Some(_) => drop(n - 1)
		case None		 => Halt()
	}
def takeWhile[I](f: I => Boolean): Proc[I, I] = 
	await {
		case Some(i) if f(i)	=> takeWhile(f)
		case Some(i)					=> emit(i, id)
		case None							=> Halt()
	}
def dropWhile[I](f: I => Boolean): Proc[I, I] =
	await {
		case Some(i) if f(i)	=> dropWhile(f)
		case Some(i)					=> emit(i, id)
		case None 						=> Halt()
	}