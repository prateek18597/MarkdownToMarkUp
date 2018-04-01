use "fileIO.sml";
open FileIO;



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

local
	fun H4([],L,ans) = ([],ans^"<h4>\n"^String.implode(List.rev(L))^"\n</h4>\n")
		| H4(h::t,L,ans)=
		if h<> #"\n" then
			H4(t,h::L,ans)
		else
			(t,ans^"<h4>\n"^String.implode(List.rev(L))^"\n</h4>\n")
in
	fun h4(s,ans)=
		H4(s,[],ans);
end
local
	fun H5([],L,ans) = ([],ans^"<h5>\n"^String.implode(List.rev(L))^"\n</h5>\n")
		| H5(h::t,L,ans)=
		if h<> #"\n" then
			H5(t,h::L,ans)
		else
			(t,ans^"<h5>\n"^String.implode(List.rev(L))^"\n</h5>\n")
in
	fun h5(s,ans)=
		H5(s,[],ans);
end

local
	fun H6([],L,ans) = ([],ans^"<h6>\n"^String.implode(List.rev(L))^"\n</h6>\n")
		| H6(h::t,L,ans)=
		if h<> #"\n" then
			H6(t,h::L,ans)
		else
			(t,ans^"<h6>\n"^String.implode(List.rev(L))^"\n</h6>\n")
in
	fun h6(s,ans)=
		H6(s,[],ans);
end
local
	fun em(h::t,L)=
		if h<> #"*" then
			em(t,h::L)
		else
			("pratik.txt","<em>\n"^String.implode(List.rev(L))^"\n</em>\n")
in
	fun Em(s)=
		em(String.explode(s),[]);
end

local
fun strong(h::t,L)=
	if h<> #"*" andalso hd(t)<> #"*" then
		strong(t,h::L)
	else
		append("pratik.txt","<strong>\n"^String.implode(List.rev(h::L))^"\n</strong>\n")
in
	fun Strong(s)=
		strong(String.explode(s),[]);
end

fun Main([],ans) = append("pratik.txt",ans)
		|	Main(h::t,ans)=
		if h= #"#" andalso hd(t)= #"#" andalso hd(tl(t))= #"#" andalso hd(tl(tl(t)))= #"#" then
					Main(tl(tl(tl(t))),h4(tl(tl(tl(t))),ans))
		else
			if h= #"#" andalso hd(t)= #"#" andalso hd(tl(t))= #"#" then
						Main(tl(tl(t)),h3(tl(tl(t)),ans))
			else
				if h= #"#" andalso hd(t)= #"#" then
						Main(h2(tl(t),ans))
				else
					if h= #"#" then
						Main(h1(t,ans))
					else
				 		append("pratik.txt",ans)

fun main(s)=
	Main(getclist(s),"")
