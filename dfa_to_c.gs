 
ctype Trans where
      Del: Int -> Char -> Int -> Trans
    
input: Int -> [Trans] -> [Int] -> Dialogue



input.start.transitions.final = writeFile."newdfa.c".(fsm.start.final ++ harray.transitions.[] ++ main ++main2).abort.done

fsm:Int -> [Int] -> String
fsm.a.b="\nint fsm(char string[120])\n{\n\tint is="++show.a++",state=-1,i=0;\n\tint nof;\n\tnof="++show.(length.b)++";\n\tint fs[] = {" ++ gy.b  ++"};\n\t" 

gy:[Int] -> String
gy.(x::[])=show.x 
gy.(x::xs)=show.x ++ "," ++ gy.xs



harray: [Trans]->[Int] -> String
harray.[].op=[]

harray.((Del.x.'X'.z)::fs).op=   ("\n\t{\n\t\ti=i+1;\n\t\tgoto q" ++ show.z ++";\n\t}")  ++ harray.fs.op 
harray.((Del.x.y.z)::ys).op  = if(x `elem` op) then ("\n\tif(string[i]=='" ++ [y] ++ "')\n\t{\n\t\ti=i+1;\n\t\tgoto q" ++ (show.z) ++ ";\n\t}") ++ harray.ys.op else  ("\nq"++show.x ++ ":\n\tif(string[i]=='\\0')\n\t{\n\t\tstate="++show.x++";\n\t\tgoto a;\n\t}")++ ("\n\tif(string[i]=='" ++ [y] ++ "')\n\t{\n\t\ti=i+1;\n\t\tgoto q" ++ (show.z) ++ ";\n\t}") ++ harray.ys.(x::op)


main= ("a:\n\t i=0; \n\t while(i<nof) \n\t { \n\t if(state==fs[i]) \n {\n\t return 1; \n \n}\ni++;\n} return 0;\n} \n\n")
main2=(" void main(int argc,char *argv[]) \n {\n\t int i=1,x; \n\t while(i<argc) \n\t {\n\t\t x=fsm(argv[i]);\n\t\t if(x==1) \n\t\t {\n\t\t\t printf(\"   %s => ACCEPTED \",argv[i]);\n\t\t }\n\t\t else \n\t\t\t printf(\"   %s => REJECTED \",argv[i]);\n\t\ti++;\n\t}}")

