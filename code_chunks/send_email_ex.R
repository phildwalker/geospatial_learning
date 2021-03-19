# sending email example

#* give email results for user
#* @serializer unboxedJSON
#* @get /userEMAIL
function(usernm="", filenm=""){
  library(glue)
  library(DBI)
  library(RDCOMClient)
  
  EDW = DBI::dbConnect(odbc::odbc(), dsn = "MCCBIEDW1")
  
  email <- dbSendQuery(EDW, glue_sql("SELECT [PersonNBR] ,[FirstNM],[LastNM],[WorkEmailAddressTXT]
  FROM [Kronos].[Employee].[Person] where personnbr in ({userID*})", 
  userID = usernm,
  .con = EDW))
  email_Output <- dbFetch(email)
  dbDisconnect(EDW)
  
  time <- Sys.time()
  
  message <- paste0(
    "<html>Hello!<br>
Thanks for submitting: <strong>", filenm, "</strong> at <strong>",time, "</strong> to the EA Knowledge Repo.<br>
Please allow for a day for the file to be uploaded, and if after that you don't see your analysis please reach out to the eaverse team. <br>
Thanks! <br>
-eaverse team</html>" 
  )
  
  OutApp <- COMCreate("Outlook.Application")
  #create an email
  outMail = OutApp$CreateItem(0)
  #configure  email parameter
  outMail[["To"]] = email_Output$WorkEmailAddressTXT
  outMail[["subject"]] = paste("Knowledge repo submission successful")
  outMail[["htmlbody"]] = message
  # sends email
  outMail$Send()  
  
  
}