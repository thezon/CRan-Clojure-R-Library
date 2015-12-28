# function makes call to clojure-R webservice

callService <- function(clojureCode){
	port<-3131
	server<-"localhost"
	param<-"cljin"
	srvTime<-2
	
	userSetTimout <-getOption("timeout")
	
	options(timeout=srvTime)
	
	rin<-tryCatch({
		conn<-url(
			URLencode(paste("http://", server, ":", port, "/?", param,"=", clojureCode,sep="")))
		 	rin<-readLines(conn, warn=FALSE)
			close(conn)
			return(rin)}
		, warn = function(w){
			return("")}
		, error = function(e){
		  	rin<-"Error: Connection failed or service is not running."
		  	return(rin)})
	
	#clean up global changes	  	
	options(timeout=userSetTimout)
	
	return(rin)
}

clojHelp <-function (command){
	print("use #(<ops> <parms>) to submit command into clojure ")
	print("use use # as escape for # thus ##(<ops> <parms>) is #(<ops> <parms>")
	print("All keywords must have ' prefix  (def 'a 3)")
	print("")

}

expEval <-function (rcode,verbose){

	result<-tryCatch({
					print(eval(parse(text=rcode)))
					return("R evaluation success")}
				, warn = function(w){
					return("R evaluation warning.")}
				, error = function(e){
					print("R evaluation error: Command did not exicute.")
					return("R evaluation error: Command did not exicute.")})
					
		if(verbose == "debug"){
			print(result)
		}
			
}

clojure <- function (x, sessionType=c("user","debug")){
	
	verbose <- match.arg(sessionType);
	
	print(verbose)
	
	print("Enter end or exit to stop Clojure processing.")
    
	repeat{
		clojureCode <- readline(prompt="clj> ")
		
		if(tolower(clojureCode) == "end" 
			|| tolower(clojureCode) == "exit"){
				 break;
			}
		if(tolower(clojureCode)=="help"){
			clojHelp()
			next
		}
		
		if(nchar(clojureCode)==0){
			print("Please Enter an Expression.")
			next;
			}
			
		if(verbose == "debug"){
			print(paste("User Entered:", clojureCode))
		}
		
		rin <- callService(clojureCode)
	
		if(verbose == "debug"){
			print(paste("Service Returned:", rin))
			}
		
		if(length(rin) != 0){	
			if("error" == tolower(substr(rin, start=0 ,stop=5))){
					print(rin,max=1000)
				}
				else{
					expEval(rin,verbose)
				}
			}
		}
	print("Clojure session has ended.")
}
