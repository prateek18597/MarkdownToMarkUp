use "fileIO.sml";
open FileIO;
(*


local
	fun H1([],L,ans) = ([],ans^"<h1>\n"^String.implode(List.rev(L))^"\n</h1>\n")
		| H1(h::t,L,ans)=
		if h<> #"\n" then
			H1(t,h::L,ans)
		else
			(t,ans^"<h1>\n"^String.implode(List.rev(L))^"\n</h1>\n")
in
	fun h1(s,ans)=
		H1(s,[],ans);
end

local
	fun H2([],L,ans) = ([],ans^"<h2>\n"^String.implode(List.rev(L))^"\n</h2>\n")
		| H2(h::t,L,ans)=
		if h<> #"\n" then
			H2(t,h::L,ans)
		else
			(t,ans^"<h2>\n"^String.implode(List.rev(L))^"\n</h2>\n")
in
	fun h2(s,ans)=
		H2(s,[],ans);
end
local
	fun H3([],L,ans) = ([],ans^"<h3>\n"^String.implode(List.rev(L))^"\n</h3>\n")
		| H3(h::t,L,ans)=
		if h<> #"\n" then
			H3(t,h::L,ans)
		else
			(t,ans^"<h3>\n"^String.implode(List.rev(L))^"\n</h3>\n")
in
	fun h3(s,ans)=
		H3(s,[],ans);
end
*)
fun H1([],ans) = ([],ans^"\n</h1>\n")
	| H1(h::t,ans)=
	if h<> #"\n" then
		H1(t,ans^Char.toString(h))
	else
		(t,ans^"\n</h1>\n")

fun H2([],ans) = ([],ans^"\n</h2>\n")
	| H2(h::t,ans)=
	if h<> #"\n" then
		H2(t,ans^Char.toString(h))
	else
		(t,ans^"\n</h2>\n")
fun H3([],ans) = ([],ans^"\n</h3>\n")
	| H3(h::t,ans)=
	if h<> #"\n" then
		H3(t,ans^Char.toString(h))
	else
		(t,ans^"\n</h3>\n")
fun H4([],ans) = ([],ans^"\n</h4>\n")
	| H4(h::t,ans)=
	if h<> #"\n" then
		H4(t,ans^Char.toString(h))
	else
		(t,ans^"\n</h4>\n")
fun H5([],ans) = ([],ans^"\n</h5>\n")
	| H5(h::t,ans)=
	if h<> #"\n" then
		H5(t,ans^Char.toString(h))
	else
		(t,ans^"\n</h5>\n")
fun H6([],ans) = ([],ans^"\n</h6>\n")
	| H6(h::t,ans)=
	if h<> #"\n" then
		H6(t,ans^Char.toString(h))
	else
		(t,ans^"\n</h6>\n")

local
	fun EM([],ans) = ([],ans^"</em>\n")
		| EM(h::t,ans)=
				if h= #"*" then
					(t,ans^"</em>\n")
				else
					EM(t,ans^Char.toString(h))
in
	fun em(s,ans)=
		EM(s,ans);
end

local
	fun Strong([],ans) = ([],ans^"</strong>")
		| Strong(h::t,ans)=
				if h= #"*" andalso hd(t)= #"*" then
					(tl(t),ans^"</strong>")
				else
					Strong(t,ans^Char.toString(h))
in
	fun strong(s,ans)=
		Strong(s,ans);
end

fun underline([],ans) = ([],ans^"</u>\n")
	| underline(h::t,ans) =
			if h= #"_" andalso Char.isSpace(hd(t)) then
				(tl(t),ans^"</u>")
			else
				underline(t,ans^Char.toString(h))
(*For Ordered List/*)

local
	fun EML([],ans) = ([],ans^"</em>\n",true)
		| EML(h::t,ans)=
				if h= #"*" then
					(t,ans^"</em>\n",true)
				else
					EML(t,ans^Char.toString(h))
in
	fun eml(s,ans)=
		EML(s,ans);
end

local
	fun StrongL([],ans) = ([],ans^"</strong>",true)
		| StrongL(h::t,ans)=
				if h= #"*" andalso hd(t)= #"*" then
					(tl(t),ans^"</strong>",true)
				else
					StrongL(t,ans^Char.toString(h))
in
	fun strongl(s,ans)=
		StrongL(s,ans);
end

fun underlinel([],ans) = ([],ans^"</u>\n",true)
	| underlinel(h::t,ans) =
			if h= #"\\"  andalso h= #"_" andalso Char.isAlpha(hd(tl(t)))=false then
				(tl(t),ans^"</u>",true)
			else
				underlinel(t,ans^Char.toString(h))
(**)

local
	fun P([],ans) = ([],ans^"\n</p>\n")
		| P(h::t,ans)=
		
			if h= #"*" andalso hd(t)= #"*" andalso Char.isAlpha(hd(tl(t))) then
				P(strong(tl(t),ans^"<strong>"))
			else
				if h= #"*" andalso Char.isAlpha(hd(t)) then
					P(em(t,ans^"<em>"))
				else
					(*if h= #"_" andalso Char.isAlpha(hd(t)) then*)
						(*P(underline(tl(t),ans^"<u>"))*)
					(*else*)
						if h<> #"\n" andalso hd(t)<> #"\n" then
							P(t,ans^Char.toString(h))
						else
							if h= #"\n" orelse h= #"\t" then
								P(t,ans)
							else
							(t,ans^"\n</p>\n")
in
	fun p(s,ans)=
		P(s,ans);
end

local
	fun LIO([],ans,check) = ([],ans^"\n</p></li>\n")
		| LIO(h::t,ans,check)=
		
		if check=false andalso Char.isDigit(h) then
			LIO(t,ans,false)
		else
			if check=false andalso h= #"." then
				LIO(tl(t),ans,true)
			else
				if h= #"*" andalso hd(t)= #"*" andalso Char.isAlpha(hd(tl(t))) then
					LIO(strongl(tl(t),ans^"<strong>"))
				else
					if h= #"*" andalso Char.isAlpha(hd(t)) then
						LIO(eml(t,ans^"<em>"))
					else
						(*if h= #"_" andalso Char.isAlpha(hd(t)) then*)
							(*LIO(underlinel(tl(t),ans^"<u>"))*)
						(*else*)
				
				if h= #"\n" andalso hd(t)= #"\n" then
					(tl(t),ans^"\n</p></li>\n")
				else
					if h= #"\n" orelse h= #"\t" then
						LIO(t,ans,check)
					else
						LIO(t,ans^Char.toString(h),true)
in
	fun lio(s,ans)=
		LIO(s,ans,false);
end

local
	fun LIU([],ans) = ([],ans^"\n</p></li>\n")
		| LIU(h::t,ans)=
				if h= #"*" andalso hd(t)= #"*" andalso Char.isAlpha(hd(tl(t))) then
					LIU(strong(tl(t),ans^"<strong>"))
				else
					if h= #"*" andalso Char.isAlpha(hd(t)) then
						LIU(em(t,ans^"<em>"))
					else
						if h= #"\n" andalso hd(t)= #"-" then
								(t,ans^"\n</p></li>\n")
							else
								if h= #"\n" orelse h= #"\t" then
									LIU(t,ans)
								else
								LIU(t,ans^Char.toString(h))
in
	fun liu(s,ans)=
		LIU(s,ans);
end


local
	fun OL([],ans) = ([],ans^"\n</ol>\n")
		| OL(h::t,ans)=
		if Char.isDigit(h) then
			OL(lio(t,ans^"<li>\n"))
		else
			(h::t,ans^"\n</ol>\n")
in
	fun ol(s,ans)=
		OL(s,ans);
end

local
	fun UL([],ans) = ([],ans^"\n</ul>\n")
		| UL(h::t,ans)=
		if h= #"-" andalso Char.isSpace(hd(t)) then
			UL(liu(tl(t),ans^"<li>\n"))
		else
			(t,ans^"\n</ul>\n")
in
	fun ul(s,ans)=
		UL(s,ans);
end

local
	fun tag([],L,ans) = ([],ans^String.implode(List.rev(L))^"\n")
		| tag(h::t,L,ans)=
				if h= #">" then
					(t,ans^String.implode(List.rev(h::L))^"\n")
				else
					tag(t,h::L,ans)
in
	fun htmltag(s,ans)=
		tag(s,[],ans);
end

fun Main([],ans) = append("pratik.txt.html",ans)
		|	Main(h::t,ans)=

		if h= #"#" andalso hd(t)= #"#" andalso hd(tl(t))= #"#" andalso hd(tl(tl(t)))= #"#" andalso hd(tl(tl(tl(t))))= #"#" andalso hd(tl(tl(tl(tl(t)))))= #"#" then
					Main(H6(tl(tl(tl(tl(tl(t))))),ans^"<h6>\n"))
		else

			if h= #"#" andalso hd(t)= #"#" andalso hd(tl(t))= #"#" andalso hd(tl(tl(t)))= #"#" andalso hd(tl(tl(tl(t))))= #"#" then
						Main(H5(tl(tl(tl(tl(t)))),ans^"<h5>\n"))
			else

				if h= #"#" andalso hd(t)= #"#" andalso hd(tl(t))= #"#" andalso hd(tl(tl(t)))= #"#" then
							Main(H4(tl(tl(tl(t))),ans^"<h4>\n"))
				else
					if h= #"#" andalso hd(t)= #"#" andalso hd(tl(t))= #"#" then
								Main(H3(tl(tl(t)),ans^"<h3>\n"))
					else
						if h= #"#" andalso hd(t)= #"#" then
								Main(H2(tl(t),ans^"<h2>\n"))
						else
							if h= #"#" then
								Main(H1(t,ans^"<h1>\n"))
							else
								if Char.isAlpha(h) then
									Main(p(h::t,ans^"<p>\n"))
						 		else
						 			if h= #"1" orelse h= #"2" orelse h= #"3" orelse h= #"4" orelse h= #"5" orelse h= #"6" orelse h= #"7" orelse h= #"8" orelse h= #"9" then
						 				Main(ol(h::t,ans^"<ol>\n"))
						 			else
						 				if h= #"-" andalso Char.isSpace(hd(t)) then 
						 					Main(ul(h::t,ans^"<ul>\n"))
						 				else
						 					if h= #"<" andalso hd(t)<> #"<" then
						 						Main(htmltag(h::t,ans))
						 					else
						 						if h= #"\n" then
						 							Main(t,ans)
						 						else	
						 							Main(t,ans^Char.toString(h))

fun main(s)=
	Main(getclist(s),"")
