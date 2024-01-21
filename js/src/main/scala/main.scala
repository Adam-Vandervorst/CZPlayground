import org.scalajs.dom

import scala.scalajs.js
import js.JSConverters.*
import be.adamv.cz2.*
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.html.Canvas


object DataParser extends Parser:
  override val empty: Int = 1
  override val singleton: Int = 2

  val symbols = RangeStorage[String](100, 1000)
  val data = RangeStorage[js.Any](2 << 24, 2 << 27)

  override def tokenizer(s: String): Expr =
    if s.head == '"' then
      if s.tail.head == '`' then
        try
          val v = js.eval(s.tail.tail.init).asInstanceOf[js.Any]
          if js.isUndefined(v) then throw RuntimeException(s"parsing ${s} is undefined")
          data.addV(v)
        catch
          case e: js.JavaScriptException => throw RuntimeException(f"${e.exception} received while parsing $s")
      else symbols.addV(s)
    else
      if s.head == '`' then
        try
          val v = js.eval(s.tail).asInstanceOf[js.Any]
          if js.isUndefined(v) then throw RuntimeException(s"parsing ${s} is undefined")
          data.addV(v)
        catch
          case e: js.JavaScriptException => throw RuntimeException(f"${e.exception} received while parsing $s")
      else symbols.addV(s)

  def sexprs(s: String)(using em: ExprMap[Unit]): Unit =
    val it = s.iterator.buffered
    var last = sexprUnsafe(it)
    while last != null do
      em.update(last, ())
      last = sexprUnsafe(it)

object GroundedPrinter extends Printer:
  val newVarString: String = "◆"
  def preVarString(x: Long): String = "⏴" + subscript(-x.toInt)
  def repr(x: Any): String =
    x.toString.flatMap {
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case other => other.toString
    }
  def freeVarString(x: Long): String =
    DataParser.symbols.get(x.toInt)
      .orElse(DataParser.data.get(x.toInt).map(repr))
      .getOrElse(x.toString)
  val exprSep: String = " "
  val exprOpen: String = "("
  val exprClose: String = ")"

//abstract class ValueEvaluationAlgorithms[V]:
//  import ExprExamples.*
//
//  def handleLookup(emv: V, ev: V): V
//  def handleMerge(fv: V, av: V): V
//
//  def lookupMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
//    val r = s.transform(Expr(`=`, e, $), Var(-e.nvarsN - 1))
//    r.map(w => handleLookup(w, v))
//
//  def lookupBackupMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
//    val nv = lookupMulti(e, v)
//    if nv.isEmpty then ExprMap(e -> v)
//    else nv
//
//  def bottomUpMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
//    e.foldMap(i => lookupBackupMulti(Var(i), v), (fem, aem) => ExprMap.from(fem.items.flatMap((f, fv) => aem.items.flatMap((a, av) => lookupBackupMulti(App(f, a), handleMerge(fv, av)).items))))
//
//  def evalMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
//    fixproject[ExprMap[V], Set[Expr]](em => ExprMap.from(em.items.flatMap(bottomUpMulti(_, _).items)), _.keys.toSet)(ExprMap[V](e -> v))
//
//  def lookup(e: Expr, v: V)(using s: ExprMap[V]): Option[(Expr, V)] =
//    val es = lookupMulti(e, v)
//    if es.size > 1 then throw RuntimeException(s"Nonlinear ${e.show}, results: ${es.keys.map(_.show).mkString(",")}")
//    else es.items.headOption
//
//  def lookupBackup(e: Expr, v: V)(using s: ExprMap[V]): (Expr, V) =
//    lookup(e, v).getOrElse(e -> v)
//
//  def bottomUp(e: Expr, v: V)(using s: ExprMap[V]): (Expr, V) =
//    e.foldMap(i => lookupBackup(Var(i), v), {case ((f, fv), (a, av)) => lookupBackup(App(f, a), handleMerge(fv, av))})
//
//  def eval(e: Expr, v: V)(using s: ExprMap[V]): (Expr, V) =
//    fixproject[(Expr, V), Expr](bottomUp, _._1)(e -> v)
//
//  def bottomUpMultiGrounded(e: Expr, v: V)(using s: ExprMap[V], g: PartialFunction[Int, ExprMap[V] => ExprMap[V]]): ExprMap[V] =
//    var newv = 0
//    var doeval = true
//    e.foldMap(i =>
//      if i > 0 then lookupBackupMulti(Var(i), v)
//      else
//        val r = ExprMap(Var(i) -> v)
//        if i == 0 then newv += 1
//        if i < -newv then doeval = false
//        r
//      ,
//      (fem, aem) =>
//        ExprMap.from(
//          fem.items.flatMap((f, fv) => f match
//            case Var(g(grounded)) =>
//              grounded(aem).items
//            case _ =>
//              aem.items.flatMap((a, av) =>
//                if doeval then lookupBackupMulti(App(f, a), handleMerge(fv, av)).items
//                else List(App(f, a) -> handleMerge(fv, av)))
//          )
//        )
//    )
//
//  def evalGrounded(em: ExprMap[V])(using s: ExprMap[V], g: PartialFunction[Int, ExprMap[V] => ExprMap[V]]): ExprMap[V] =
//
//    fixproject[ExprMap[V], Set[Expr]](em => ExprMap.from(em.items.flatMap(bottomUpMultiGrounded(_, _).items)), _.keys.toSet)(em)
//
//  def evalGrounded(e: Expr, v: V)(using s: ExprMap[V], g: PartialFunction[Int, ExprMap[V] => ExprMap[V]]): ExprMap[V] =
//    evalGrounded(ExprMap[V](e -> v))
//
//  def evalGroundedN(e: Expr, v: V, N: Int)(using s: ExprMap[V], g: PartialFunction[Int, ExprMap[V] => ExprMap[V]]): ExprMap[V] =
//    var s = ExprMap[V](e -> v)
//    for _ <- 1 to N do
//      s = ExprMap.from(s.items.flatMap(bottomUpMultiGrounded(_, _).items))
//    s
//
//  def apply(e: Expr)(using s: ExprMap[V]): ExprMap[V]
//
//  def unapply(e: Expr)(using s: ExprMap[V]): Option[(Expr, V)]


extension (inline em: ExprMap[Unit])
  inline def varsIt: Iterator[Int] = if em.em eq null then Iterator.empty else em.em.vars.keys.iterator.map(_.toInt)

  inline def varMap(inline f: Int => Int, inline maintain: Boolean): ExprMap[Unit] =
    if em.em eq null then em
    else
      val ks = em.em.vars.keys.toArray.map(x => f(x.toInt).toLong)
      val vs = Array.fill[Unit](ks.length)(())
      val lm = collection.mutable.LongMap.fromZip[Unit](ks, vs)
      inline if maintain then ExprMap(EM(em.em.apps, lm))
      else ExprMap(EM(ExprMap(), lm))


  inline def varFlatMap(inline f: Int => IterableOnce[Int], inline maintain: Boolean): ExprMap[Unit] =
    if em.em eq null then em
    else
      val ks = em.em.vars.keys.toArray.flatMap(x => f(x.toInt).iterator.map(_.toLong))
      val vs = Array.fill[Unit](ks.length)(())
      val lm = collection.mutable.LongMap.fromZip[Unit](ks, vs)
      inline if maintain then ExprMap(EM(em.em.apps, lm))
      else ExprMap(EM(ExprMap(), lm))

  inline def varFilter(inline f: Int => Boolean, inline maintain: Boolean): ExprMap[Unit] =
    if em.em eq null then em
    else
      val ks = em.em.vars.keys.toArray.filter(x => f(x.toInt))
      val vs = Array.fill[Unit](ks.length)(())
      val lm = collection.mutable.LongMap.fromZip[Unit](ks, vs)
      inline if maintain then ExprMap(EM(em.em.apps, lm))
      else ExprMap(EM(ExprMap(), lm))



@main def m =
  val root = dom.document.querySelector("#board")

  dom.window.asInstanceOf[js.Dynamic].ctx = root.firstElementChild.asInstanceOf[Canvas].getContext("2d")

  var is_fullscreen = false;
  dom.window.addEventListener("keydown", k => {
    println(k);
    if k.asInstanceOf[KeyboardEvent].key == "f" then
      if is_fullscreen then dom.document.exitFullscreen()
      else dom.window.asInstanceOf[js.Dynamic].ctx.canvas.requestFullscreen()
      is_fullscreen = !is_fullscreen
  })

  val ev = ValueEvaluationAlgorithms.ignore[Unit]
  given em: ExprMap[Unit]()

  val eq_id = DataParser.symbols.add("=")
  val transform_id = DataParser.symbols.add("transform")
  val ground_id = DataParser.symbols.add("ground")
  val pretty_id = DataParser.symbols.add("pretty")
  em.update(Expr(Var(eq_id), DataParser.symbols.addV("root"), DataParser.data.addV(root)), ())

  val pfs = collection.mutable.Map.empty[Int, ExprMap[Unit] => ExprMap[Unit]]
  var pc = 10000
  given PartialFunction[Int, ExprMap[Unit] => ExprMap[Unit]] = {
    case `ground_id` => _.varMap({ i => // name
        pc += 1
        val name = DataParser.symbols.get(i).get
        println(f"grounding ${name} ${i}")
        val cb_id = pc
        pfs(pc) = cb =>
          println(f"cb ${cb.prettyStructuredSet()} ${cb_id}")
          cb.varFilter(!DataParser.data.couldContain(_), true).execute(Iterator.single(Instr.Apply(cb_id))) union cb.varFilter(DataParser.data.couldContain, false).varMap({ j => // function
          val ev = DataParser.data.get(j).get
          println(f"got ${DataParser.data.get(j)} for ${j}")
          if js.typeOf(ev) != "function" then
            println("error")
            println(pfs)
            println(DataParser.symbols.indexToValue)
            println(DataParser.data.indexToValue)
          assert(js.typeOf(ev) == "function")
          val func = ev.asInstanceOf[js.Function]
//          val base = pfs.getOrElse(i, _ => ExprMap())
          pfs(i) = func.length match
            case 1 =>
              println(s"lifting single argument ${name}")
              arg1 => arg1.varFilter(!DataParser.data.couldContain(_), true).execute(Iterator.single(Instr.Apply(i))) union {
              val ar1 = arg1.varsIt.collect{ case k if DataParser.data.couldContain(k) => DataParser.data.get(k).get }.toJSArray
              val obj = func.call(js.Object(), ar1)
              println(s"${name}(arg1=${ar1}) = ${obj}")
              val ks = obj.asInstanceOf[js.Array[js.Any]].map(v =>
                if js.isUndefined(v) then throw RuntimeException(s"got undefined executing ${func} ${name} on ${ar1} ${js.typeOf(ar1(0))}")
                DataParser.data.add(v).toLong
              )
              val vs = Array.fill[Unit](ks.length)(())
              val lm = collection.mutable.LongMap.fromZip[Unit](ks, vs)
              ExprMap(EM(ExprMap(), lm))
            }
            case 2 =>
              println(s"lifting two arguments ${name}")
              arg1 => {
              pc += 1
              val arg2_id = pc
//              println(s"${name} received argument 1 ${arg1.keys.map(GroundedPrinter.sexpression(_)).mkString("; ")}, now waiting on ${arg2_id}")
              pfs(pc) = arg2 => arg2.varFilter(!DataParser.data.couldContain(_), true).execute(Iterator.single(Instr.Apply(arg2_id))) union {
//                println(s"${name} received argument 2 ${arg2.keys.map(GroundedPrinter.sexpression(_)).mkString("; ")}")
                val ar1 = arg1.varsIt.collect{ case k if DataParser.data.couldContain(k) => DataParser.data.get(k).get }.toJSArray
                val ar2 = arg2.varsIt.collect{ case k if DataParser.data.couldContain(k) => DataParser.data.get(k).get }.toJSArray
                val jks = func.call(js.Object(), ar1, ar2).asInstanceOf[js.Array[js.Any]]
//                println(s"${name}(arg1=${ar1}, arg2=${ar2}) = ${jks}")
                val ks = jks.map(v => DataParser.data.add(v).toLong)
                val vs = Array.fill[Unit](ks.length)(())
                val lm = collection.mutable.LongMap.fromZip[Unit](ks, vs)
                ExprMap(EM(ExprMap(), lm))
              }
              arg1.varFilter(!DataParser.data.couldContain(_), true).execute(Iterator.single(Instr.Apply(i))) union ExprMap.single(Var(pc), ())
            }
            case 3 =>
              println(s"lifting three arguments ${name}")
              arg1 => {
              pc += 1
              val arg2_id = pc
              pfs(pc) = arg2 => {
                pc += 1
                val arg3_id = pc
                pfs(pc) = arg3 => {
                  arg3.varFilter(!DataParser.data.couldContain(_), true).execute(Iterator.single(Instr.Apply(arg3_id))) union {
                    val ar1 = arg1.varsIt.collect { case k if DataParser.data.couldContain(k) => DataParser.data.get(k).get }.toJSArray
                    val ar2 = arg2.varsIt.collect { case k if DataParser.data.couldContain(k) => DataParser.data.get(k).get }.toJSArray
                    val ar3 = arg3.varsIt.collect { case k if DataParser.data.couldContain(k) => DataParser.data.get(k).get }.toJSArray
                    val ks = func.call(js.Object(), ar1, ar2, ar3).asInstanceOf[js.Array[js.Any]].map(v => DataParser.data.add(v).toLong)
                    val vs = Array.fill[Unit](ks.length)(())
                    val lm = collection.mutable.LongMap.fromZip[Unit](ks, vs)
                    ExprMap(EM(ExprMap(), lm))
                  }
                }
                arg2.varFilter(!DataParser.data.couldContain(_), true).execute(Iterator.single(Instr.Apply(arg2_id))) union ExprMap.single(Var(pc), ())
              }
              arg1.varFilter(!DataParser.data.couldContain(_), true).execute(Iterator.single(Instr.Apply(i))) union ExprMap.single(Var(pc), ())
            }
            case 4 =>
              arg1 => {
              pc += 1
              val arg2_id = pc
              pfs(pc) = arg2 => {
                pc += 1
                val arg3_id = pc
                pfs(pc) = arg3 => {
                  pc += 1
                  val arg4_id = pc
                  pfs(pc) = arg4 => {
                    arg4.varFilter(!DataParser.data.couldContain(_), true).execute(Iterator.single(Instr.Apply(arg4_id))) union {
                      val ar1 = arg1.varsIt.collect { case k if DataParser.data.couldContain(k) => DataParser.data.get(k).get }.toJSArray
                      val ar2 = arg2.varsIt.collect { case k if DataParser.data.couldContain(k) => DataParser.data.get(k).get }.toJSArray
                      val ar3 = arg3.varsIt.collect { case k if DataParser.data.couldContain(k) => DataParser.data.get(k).get }.toJSArray
                      val ar4 = arg4.varsIt.collect { case k if DataParser.data.couldContain(k) => DataParser.data.get(k).get }.toJSArray
                      val ks = func.call(js.Object(), ar1, ar2, ar3, ar4).asInstanceOf[js.Array[js.Any]].map(v => DataParser.data.add(v).toLong)
                      val vs = Array.fill[Unit](ks.length)(())
                      val lm = collection.mutable.LongMap.fromZip[Unit](ks, vs)
                      ExprMap(EM(ExprMap(), lm))
                    }
                  }
                  arg3.varFilter(!DataParser.data.couldContain(_), true).execute(Iterator.single(Instr.Apply(arg3_id))) union ExprMap.single(Var(pc), ())
                }
                arg2.varFilter(!DataParser.data.couldContain(_), true).execute(Iterator.single(Instr.Apply(arg2_id))) union ExprMap.single(Var(pc), ())
              }
              arg1.varFilter(!DataParser.data.couldContain(_), true).execute(Iterator.single(Instr.Apply(i))) union ExprMap.single(Var(pc), ())
            }
          j
        }, false)
        pc
      }, false)
    case `pretty_id` => space =>
//      println(s"pretty ${space.keys.map(GroundedPrinter.sexpression(_)).mkString("; ")}")
      ExprMap.from(space.keys.map(e1 =>
        DataParser.data.addV(GroundedPrinter.sexpression(e1, colored=false)) -> ()
      ))
    case `transform_id` => pattern =>
      ExprMap.from(pattern.items.map((e1, s1) =>
        pc += 1
        pfs(pc) = template => ExprMap.from(template.items.flatMap((e2, s2) =>
          em.transform(e1, e2).items
        ))
        Var(pc) -> ()
      ))
    case pfs(handler) => handler
  }




//(= (draw (Text $t (Point2D $x $y))) (fillText (pretty $t) $x (* `15 $y)))
  var counter = 0

  def evalAdd(s: String): Unit =
    val parsed = DataParser.sexpr(s.iterator).get
    em.update(Expr(DataParser.symbols.addV("userOp"), DataParser.data.addV(counter), DataParser.symbols.addV("evalAdd"), DataParser.data.addV(s)), ())
    val res = ev.evalGrounded(parsed, ())
    if res.em ne null then em.em = res.em union em.em
    println(f"evaluated ${res.keys.map(GroundedPrinter.sexpression(_)).mkString("; ")}")
    counter += 1

  """
    |(= (positions $n) (Point2D `0 (* (transform (FONT_SIZE $x) (* `1.2 $x)) (upto $n))))
    |(= (++ (Point2D $x1 $y1) (Point2D $x2 $y2)) (Point2D (+ $x1 $x2) (+ $y1 $y2)))
    |(= (draw (Text $t (Point2D $x $y))) (fillText $t $x $y))
    |(= (draw (Rect (Point2D $x $y) (Point2D $w $h))) (fillRect $x $y $w $h))
    |""".stripMargin.strip().split("\n").foreach(evalAdd)

  evalAdd("(ground lift \"`fs => fs.map(f => xs => xs.map(f))\")")
  evalAdd("(ground lift_ \"`fs => fs.map(f => xs => xs.flatMap(f))\")")
  evalAdd("(ground lift2 \"`fs => fs.map(f => (xs, ys) => xs.flatMap(x => ys.map(y => f(x, y))))\")")
  evalAdd("(ground lift2_ \"`fs => fs.map(f => (xs, ys) => xs.flatMap(x => ys.flatMap(y => f(x, y))))\")")
  evalAdd("(ground lift3 \"`fs => fs.map(f => (xs, ys, zs) => xs.flatMap(x => ys.flatMap(y => zs.map(z => f(x, y, z)))))\")")
  evalAdd("(ground lift3_ \"`fs => fs.map(f => (xs, ys, zs) => xs.flatMap(x => ys.flatMap(y => zs.flatMap(z => f(x, y, z)))))\")")
  evalAdd("(ground lift4 \"`fs => fs.map(f => (xs, ys, zs, ws) => xs.flatMap(x => ys.flatMap(y => zs.map(z => ws.map(w => f(x, y, z, w))))))\")")
  evalAdd("(ground lift4_ \"`fs => fs.map(f => (xs, ys, zs, ws) => xs.flatMap(x => ys.flatMap(y => zs.flatMap(z => ws.flatMap(w => f(x, y, z, w))))))\")")
  evalAdd("(ground ² (lift \"`x => x*x\"))")
  evalAdd("(² `.1)")
  evalAdd("(ground + (lift2 \"`(x, y) => x+y\"))")
  evalAdd("(ground * (lift2 \"`(x, y) => x*y\"))")
  evalAdd("(* `.1 `.05)")
  evalAdd("(ground range (lift3_ \"`(start, stop, step) => Array.from({ length: (stop - start) / step }, (_, i) => start + (i * step))\"))")
  evalAdd("(range `0 `10 `1)")
//  evalAdd("(ground count \"`xs => [xs.length]\")")
//  evalAdd("(count (range `0 `10 `1))")
//  evalAdd("(ground randomInt (lift2 \"`(min, max) => min + Math.floor(Math.random() * (max - min + 1))\"))")
//  evalAdd("(randomInt `1 `100)")
//  evalAdd("""(ground sample "`(ns, ar) => ns.flatMap(n => {
//            |  const sample = ar.slice();
//            |  const last = ar.length - 1;
//            |  for (let index = 0; index < n; index++) {
//            |    const rand = index + Math.floor(Math.random() * (last - index + 1));
//            |    const temp = sample[index];
//            |    sample[index] = sample[rand];
//            |    sample[rand] = temp;
//            |  }
//            |  return sample.slice(0, n);
//            |})")""".stripMargin)
//  evalAdd("(sample `3 (range `0 `10 `1))")
//  evalAdd("(+ A B)")
//  evalAdd("(f `3)")
//  evalAdd("(* `1.2 `12)")
//  evalAdd("(* `3.14 (range `0 $n `1))")
//  evalAdd("(upto `3)")
//  evalAdd("(² (upto `3))")
//  evalAdd("(* `1.5 (upto `3))")
//  evalAdd("(Q (upto `3) `1.5)")
//  evalAdd("(* (upto `3) `1.5)")
//  evalAdd("(transform (FONT_SIZE $x) (* `1.2 $x))")
//  evalAdd("(* (transform (FONT_SIZE $x) (* `1.2 $x)) (upto `3))")
//  evalAdd("(positions `3)")
//  evalAdd("(ground logged \"`xs => {console.log('g', xs); return xs; }\")")
//  evalAdd("(logged `\"test\")")
//  evalAdd("(transform (CORNER $x) $x)")
//  evalAdd("(++ (transform (CORNER $x) $x) (positions `3))")
//  evalAdd("(ground replaceChildren \"`(xs, ys) => xs.forEach(x => x.replaceChildren(...ys)) || xs\")")
//  evalAdd("(ground appendChild (lift2 \"`(x, y) => x.appendChild(y)\"))")
//  evalAdd("(ground TextNode (lift \"`x => document.createTextNode(x)\"))")
//  evalAdd("(ground TextNode (lift \"`x => document.createTextNode(x)\"))")
//  evalAdd("(appendChild root (TextNode `\"Test\"))")
//  evalAdd("(replaceChildren root (TextNode (+ (pretty (transform (userOp $i $m $x) ($i $x))) `'\\n')))")
//  evalAdd("(appendChild root (TextNode (+ (pretty (transform (userOp $i $m $x) $x)) `'\\n')))")
  evalAdd("(ground fillText (lift3_ \"`(t, x, y) => ctx.fillText(t, x, y) || []\"))")
  evalAdd("(ground fillRect (lift4_ \"`(x, y, w, h) => ctx.fillRect(x, y, w, h) || []\"))")
  evalAdd("""(ground resetCanvas "`_ => {
            |  const pixel_ratio = window.devicePixelRatio || 1
            |  ctx.canvas.width = window.innerWidth*pixel_ratio
            |  ctx.canvas.height = window.innerHeight*pixel_ratio
            |  ctx.canvas.style.width = window.innerWidth + 'px'
            |  ctx.canvas.style.height = window.innerHeight + 'px'
            |  ctx.scale(pixel_ratio, pixel_ratio)
            |  return []
            |}")""".stripMargin)
  evalAdd("(resetCanvas go)")
  evalAdd("(ground eval (lift \"`x => eval(x)\"))")
  evalAdd("""(= WIDTH (eval `"ctx.canvas.width"))""")
  evalAdd("""(= HEIGHT (eval `"ctx.canvas.height"))""")
  evalAdd("""(eval "`\"ctx.fillStyle = '#222020'\"")""")
  evalAdd("(draw (Rect (Point2D `0 `0) (Point2D WIDTH HEIGHT)))")
  evalAdd("""(eval "`\"ctx.fillStyle = '#f0f0f0'\"")""")
  evalAdd("""(eval "`\"ctx.font = '14px system-ui'\"")""")
  evalAdd("(draw (transform (userOp $i $m $x) (Text $x (Point2D `40 (+ `50 (* `20 $i))))))")

//  println(ev.evalGrounded(DataParser.sexpr("(eval \"12\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"3.14\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"\\\"test\\\"\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"x => x * x\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"x => x * x\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(eval \"2*4\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(ground ² \"`x => Math.pow(x, 2)\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(² `0.1)".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(ground * \"`(x, y) => x*y\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(* `0.1 `0.05)".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(ground + \"`(x, y) => x+y\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded(DataParser.sexpr("(+ `\"Hello\" `\"World\")".iterator).get, ()).prettyListing(false))
//  println(ev.evalGrounded({
//    given arg: ExprMap[Unit]()
//    DataParser.sexprs(
//      """
//        |(+ `"Hello" `"World")
//        |(+ `"Hi" `"World")
//        |(+ `"Hello" `"Universe")
//        |(+ `"Hi" `"Universe")
//        |""".stripMargin)
//    println(arg.prettyListing())
//    println(arg.prettyStructuredSet())
//    arg
//  }).prettyListing(false))
  println(pfs)
  println(DataParser.symbols.indexToValue)
  println(DataParser.data.indexToValue)

