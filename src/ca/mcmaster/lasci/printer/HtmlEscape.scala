package ca.mcmaster.lasci.printer

object HtmlEscape {
	def apply(s: String) = {
		val out = new StringBuilder
		for(i <- 0 until s.length) {
			s.charAt(i) match {
				case '>' => out.append("&gt;")
				case '&' => out.append("&amp;")
				case '<' => out.append("&lt;")
				case '"' => out.append("&quot;")
				case c => out.append(c)
			}
		}
		out.toString
	}
}
        