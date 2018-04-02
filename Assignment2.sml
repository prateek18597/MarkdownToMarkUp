use "fileIO.sml";
open FileIO;

fun H1([],ans) = ([],ans^"\n</h1>\n")
	| H1(h::t,ans)=
	if h<> #"\n" then
		H1(t,ans^Char.toString(h))
	else
		if h= #"\t" orelse h= #"\\" then
								H1(t,ans)
							else
		(t,ans^"\n</h1>\n")

fun H2([],ans) = ([],ans^"\n</h2>\n")
	| H2(h::t,ans)=
	if h<> #"\n" then
		H2(t,ans^Char.toString(h))
	else
		if h= #"\t" orelse h= #"\\" then
								H2(t,ans)
							else

		(t,ans^"\n</h2>\n")
fun H3([],ans) = ([],ans^"\n</h3>\n")
	| H3(h::t,ans)=
	if h<> #"\n" then
		H3(t,ans^Char.toString(h))
	else
		if h= #"\t" orelse h= #"\\" then
								H3(t,ans)
							else
		(t,ans^"\n</h3>\n")
fun H4([],ans) = ([],ans^"\n</h4>\n")
	| H4(h::t,ans)=
	if h<> #"\n" then
		H4(t,ans^Char.toString(h))
	else
		if h= #"\t" orelse h= #"\\" then
								H4(t,ans)
							else
		(t,ans^"\n</h4>\n")
fun H5([],ans) = ([],ans^"\n</h5>\n")
	| H5(h::t,ans)=
	if h<> #"\n" then
		H5(t,ans^Char.toString(h))
	else
		if h= #"\t" orelse h= #"\\" then
								H5(t,ans)
							else
		(t,ans^"\n</h5>\n")
fun H6([],ans) = ([],ans^"\n</h6>\n")
	| H6(h::t,ans)=
	if h<> #"\n" then
		H6(t,ans^Char.toString(h))
	else
		if h= #"\t" orelse h= #"\\" then
								H6(t,ans)
							else
		(t,ans^"\n</h6>\n")

local
	fun EM([],ans,qu) = ([],ans^"</em>\n")
		| EM(h::t,ans,qu)=
				if h= #"*" then
					(t,ans^"</em>\n")
				else
					if h= #"\n" orelse h= #"\t" orelse h= #"\\" then
								EM(t,ans,qu)
							else
								if h= #"\"" andalso qu=false  then
									EM(t,ans^"<q>\n",true)
								else
									if h= #"\"" andalso qu=true  then
									EM(t,ans^"</q>\n",false)
								else
					EM(t,ans^Char.toString(h),qu)
in
	fun em(s,ans)=
		EM(s,ans,false);
end

local
	fun Strong([],ans,qu) = ([],ans^"</strong>")
		| Strong(h::t,ans,qu)=
				if h= #"*" andalso hd(t)= #"*" then
					(tl(t),ans^"</strong>")
				else
					if h= #"\n" orelse h= #"\t" orelse h= #"\\" then
								Strong(t,ans,qu)
							else
								if h= #"\"" andalso qu=false  then
									Strong(t,ans^"<q>\n",true)
								else
									if h= #"\"" andalso qu=true  then
									Strong(t,ans^"</q>\n",false)
								else
					Strong(t,ans^Char.toString(h),qu)
in
	fun strong(s,ans)=
		Strong(s,ans,false);
end

fun underline([],ans) = ([],ans^"</u>\n")
	| underline(h::t,ans) =
			if h= #"_" andalso Char.isSpace(hd(t)) then
				(t,ans^"</u>")
			else
				if h= #"\n" orelse h= #"\t" orelse h= #"\\" then
								underline(t,ans)
							else
								
				underline(t,ans^Char.toString(h))
(*For Ordered List/*)

local
	fun EML([],ans,qu) = ([],ans^"</em>\n",true)
		| EML(h::t,ans,qu)=
				if h= #"*" then
					(t,ans^"</em>\n",true)
				else
					if h= #"\n" orelse h= #"\t" orelse h= #"\\" then
								EML(t,ans,qu)
							else
								if h= #"\"" andalso qu=false  then
									EML(t,ans^"<q>\n",true)
								else
									if h= #"\"" andalso qu=true  then
									EML(t,ans^"</q>\n",false)
								else
					EML(t,ans^Char.toString(h),qu)
in
	fun eml(s,ans)=
		EML(s,ans,false);
end

local
	fun StrongL([],ans,qu) = ([],ans^"</strong>",true)
		| StrongL(h::t,ans,qu)=
				if h= #"*" andalso hd(t)= #"*" then
					(tl(t),ans^"</strong>",true)
				else
					if h= #"\n" orelse h= #"\t" orelse h= #"\\" then
								StrongL(t,ans,qu)
							else
								if h= #"\"" andalso qu=false  then
									StrongL(t,ans^"<q>\n",true)
								else
									if h= #"\"" andalso qu=true  then
									StrongL(t,ans^"</q>\n",false)
								else
					StrongL(t,ans^Char.toString(h),qu)
in
	fun strongl(s,ans)=
		StrongL(s,ans,false);
end

fun underlinel([],ans) = ([],ans^"</u>\n",true)
	| underlinel(h::t,ans) =
			if h= #"_" andalso Char.isAlpha(hd(t))=false then
				(t,ans^"</u>",true)
			else
				if h= #"\n" orelse h= #"\t" orelse h= #"\\" then
								underlinel(t,ans)
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
					if h= #"_" andalso Char.isAlpha(hd(t)) then
						P(underline(t,ans^"<u>"))
					else
						if h<> #"\n" andalso hd(t)<> #"\n" then
							P(t,ans^Char.toString(h))
						else
							if h= #"\n" orelse h= #"\t" orelse h= #"\\" then
								P(t,ans)
							else
							(t,ans^"\n</p>\n")
in
	fun p(s,ans)=
		P(s,ans);
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
						if h= #"_" andalso Char.isAlpha(hd(t))=true then
							LIU(underline(t,ans^"<u>"))
						else
							if h= #"\n" andalso hd(t)= #"\t" andalso hd(tl(t))= #"-" then
									(tl(t),ans^"\n</p></li>\n")
								else
									if h= #"\n" orelse h= #"\t" orelse h= #"\\" then
										LIU(t,ans)
									else
										LIU(t,ans^Char.toString(h))
in
	fun liu(s,ans)=
		LIU(s,ans);
end

local
	fun UL([],ans) = ([],ans^"\n</ul>\n")
		| UL(h::t,ans)=
		if h= #"-" andalso Char.isSpace(hd(t)) then
			UL(liu(tl(t),ans^"<li><p>\n"))
		else
			(t,ans^"\n</ul>\n")
in
	fun ul(s,ans)=
		UL(s,ans);
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
						if h= #"_" andalso Char.isAlpha(hd(t))=true then
							LIU(underline(t,ans^"<u>"))
						else
							if h= #"\n" andalso hd(t)= #"\t" andalso hd(tl(t))= #"-" then
									(tl(t),ans^"\n</p></li>\n")
								else
									if h= #"\n" orelse h= #"\t" orelse h= #"\\" then
										LIU(t,ans)
									else
										LIU(t,ans^Char.toString(h))
in
	fun liu(s,ans)=
		LIU(s,ans);
end

local
	fun UL([],ans) = ([],ans^"\n</ul>\n",true)
		| UL(h::t,ans)=
		if h= #"-" andalso Char.isSpace(hd(t)) then
			UL(liu(tl(t),ans^"<li><p>\n"))
		else
			(t,ans^"\n</ul>\n",true)
in
	fun ull(s,ans)=
		UL(s,ans);
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
						if h= #"_" andalso Char.isAlpha(hd(t))=true then
							LIO(underlinel(t,ans^"<u>"))
						else
						if h= #"\n" andalso hd(t)= #"\n" andalso hd(tl(t))= #"\t" then
							LIO(ull(tl(tl(t)),ans^"<ul>\n"))
						else
				
				if h= #"\n" andalso hd(t)= #"\n" then
					(tl(t),ans^"\n</p></li>\n")
				else
					if h= #"\n" orelse h= #"\t" orelse h= #"\\" then
						LIO(t,ans,check)
					else
						LIO(t,ans^Char.toString(h),true)
in
	fun lio(s,ans)=
		LIO(s,ans,false);
end



local
	fun OL([],ans) = ([],ans^"\n</ol>\n")
		| OL(h::t,ans)=
		if Char.isDigit(h) then
			OL(lio(t,ans^"<li><p>\n"))
		else
			(h::t,ans^"\n</ol>\n")
in
	fun ol(s,ans)=
		OL(s,ans);
end



local
	fun tag([],L,ans) = ([],ans^String.implode(List.rev(L))^"\n")
		| tag(h::t,L,ans)=
				if h= #">" then
					(t,ans^String.implode(List.rev(h::L))^"\n")
				else
					if h= #"\n" orelse h= #"\t" orelse h= #"\\" then
								tag(t,L,ans)
							else
					tag(t,h::L,ans)
in
	fun htmltag(s,ans)=
		tag(s,[],ans);
end

fun Main([],ans) = ans
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
						 						if h= #"\n" orelse h= #"\t" orelse h= #"\\" then
													Main(t,ans)
						 						else	
						 							Main(t,ans^Char.toString(h))

fun main(s)=
	append(s^".html",Main(getclist(s),""))
