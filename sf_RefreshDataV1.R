

# Code to Query SFDC
querySFDC <- function(x, filename, db_connStr) {
     print(filename)
     sqlQueryLoaded <- readLines(filename, encoding = "UTF-8")
     sqlQueryLoaded <- iconv(sqlQueryLoaded, to = "ASCII//TRANSLIT")
     queryString2   <- gsub("[\r\n\t]", "", paste(sqlQueryLoaded, collapse = " "))
     queryString2 <- gsub("[\r\n\t]", "", paste(sqlQueryLoaded, collapse = " "))
     
     dbhandle <-  RODBC::odbcDriverConnect(db_connStr)
     sf_AllData <- data.table::as.data.table(RODBC::sqlQuery(dbhandle, queryString2, stringsAsFactors = FALSE))
     return (sf_AllData)
}

       #sf_AllCampaigns <- refreshSFDC_DataMULTI(Campaigns = TRUE, SQLDirStrings = c("sqlGlobal", "querySFDC"), SaveResults = TRUE)

          # sf_AllAccounts <- refreshSFDC_DataMULTI(Accts = TRUE, SQLDirStrings = c("sqlGlobal", "querySFDC"), SaveResults = TRUE)
          # sf_AllLineItems <- refreshSFDC_DataMULTI(OppLineItem = TRUE, SQLDirStrings = c("sqlGlobal", "querySFDC"), SaveResults = TRUE)
          
          
          refreshSFDC_DataMULTI <- function( 
                                             Campaigns = FALSE,      CampSQL = "4_Salesforce_CAMPAIGNS_R1.sql",     
                                             Events = FALSE,         EvntsSQL = NA,
                                             Tasks = FALSE,          TsksSQL = NA,
                                             CampMbrs = FALSE,       CmpMbrsSQL = NA,
                                             Opps = FALSE,           OppsSQL = NA,
                                             Accts = FALSE,          AcctsSQL = "2_Salesforce_ACCOUNT_R3.sql",
                                             Contacts = FALSE,       CntctsSQL = NA,
                                             Users = FALSE,          UsrsSQL = NA,
                                             Leads = FALSE,          LeadsSQL = NA,
                                             ZipCodes = FALSE,       ZipSQL = NA,
                                             OppLineItem = FALSE,    OppLnItmSQL = "SFDC_OppLineItemToOppAcctJoinedV1.sql",
                                             OppHistory = FALSE,     OppHistSQL = NA,
                                             OppFieldHist = FALSE,   OppFldHstSQL = NA,
                                             ProductData = FALSE,    ProductDataSQL = "SFDC_Products2.sql",
                                             
                                             SaveResults = FALSE,
                                             LoadSF_Data = FALSE,
                                             SQLDirStrings = c("sqlGlobal", "querySFDC")) {
               
               dataDirectory <- "~/rProj_DataSciMain/dataGlobal"
               
               # Campaign Tables from SFDC: **********************************************************
               if (Campaigns) {
                    if(!LoadSF_Data) {
                         sqlfileName <- paste( "~/rProj_DataSciMain",paste(SQLDirStrings, collapse = '/'),CampSQL, sep = "/")
                         print(sqlfileName)
                         
                         sf_AllCampaigns <- querySFDC(filename = sqlfileName, db_connStr = db_connectionString)
                         rownames(sf_AllCampaigns) <- sf_AllCampaigns$Id
                         data.table::setkey(sf_AllCampaigns, Id)
                         if (SaveResults) {
                             
                              save(sf_AllCampaigns, file = paste(dataDirectory, "sf_AllCampaigns.rda", sep = "/"))
                         }
                         
                         return(sf_AllCampaigns)
                         
                    } else {
                         load(file = paste(dataDirectory, "sf_AllCampaigns.rda", sep = "/"))
                    }
                    
               }
               
               # Account Tables from SFDC: **********************************************************
               if (Accts) {
                    if(!LoadSF_Data) {
                         sqlfileName <- paste( "~/rProj_DataSciMain",paste(SQLDirStrings, collapse = '/'),AcctsSQL, sep = "/")
                         print(sqlfileName)
                         
                         sf_All_Accounts <- querySFDC(filename = sqlfileName, db_connStr = db_connectionString)
                         rownames(sf_All_Accounts) <- sf_All_Accounts$Id
                         data.table::setkey(sf_All_Accounts, Id)
                         if (SaveResults) {
                              
                              save(sf_All_Accounts, file = paste(dataDirectory, "sf_All_Accounts.rda", sep = "/"))
                         }
                         
                         return(sf_All_Accounts)
                         
                    } else {
                         load(file = paste(dataDirectory, "sf_All_Accounts.rda", sep = "/"))
                    }
                    
               }
               
               
               # Opp Line Item Tables from SFDC: **********************************************************
               if (OppLineItem) {
                    if(!LoadSF_Data) {
                         sqlfileName <- paste( "~/rProj_DataSciMain",paste(SQLDirStrings, collapse = '/'),OppLnItmSQL, sep = "/")
                         print(sqlfileName)
                         
                         sf_OppLineItemToOpp <- querySFDC(filename = sqlfileName, db_connStr = db_connectionString)
                         rownames(sf_OppLineItemToOpp) <- sf_OppLineItemToOpp$Id
                         data.table::setkey(sf_OppLineItemToOpp, Id)
                         if (SaveResults) {
                              
                              save(sf_OppLineItemToOpp, file = paste(dataDirectory, "sf_OppLineItemToOpp.rda", sep = "/"))
                         }
                         
                         return(sf_OppLineItemToOpp)
                         
                    } else {
                         load(file = paste(dataDirectory, "sf_OppLineItemToOpp.rda", sep = "/"))
                    }
                    
               }
               
               
               # Product Data Tables from SFDC: **********************************************************
               if (ProductData) {
                    if(!LoadSF_Data) {
                         
                         
                         sqlfileName <- paste( "~/rProj_DataSciMain",paste(SQLDirStrings, collapse = '/'),ProductDataSQL, sep = "/")
                         print(sqlfileName)
                         
                         sf_All_Products <- querySFDC(filename = sqlfileName, db_connStr = db_connectionString)
                         rownames(sf_All_Products) <- sf_All_Products$Id
                         data.table::setkey(sf_All_Products, Id)
                         if (SaveResults) {
                              
                              save(sf_All_Products, file = paste(dataDirectory, "sf_All_Products.rda", sep = "/"))
                         }
                         
                         return(sf_All_Products)
                         
                    } else {
                         load(file = paste(dataDirectory, "sf_All_Products.rda", sep = "/"))
                    }
                    
               }
              
               
               
               
               
               
               
                
          }
          
          
          

          
          
          
          
             
     # 
     # 
     # 
     # 
     # 
     # Campaigns <- FALSE;      CampSQL <- "4_Salesforce_CAMPAIGNS_R1.sql";     
     # Events <- FALSE;         EvntsSQL <- NA;
     # Tasks <- FALSE;          TsksSQL <- NA;
     # CampMbrs <- FALSE;       CmpMbrsSQL <- NA;
     # Opps <- FALSE;           OppsSQL <- NA;
     # Accts <- FALSE;          AcctsSQL <- NA;
     # Contacts <- FALSE;       CntctsSQL <- NA;
     # Users <- FALSE;          UsrsSQL <- NA;
     # Leads <- FALSE;          LeadsSQL <- NA;
     # ZipCodes <- FALSE;       ZipSQL <- NA;
     # OppLineItem <- FALSE;    OppLnItmSQL <- NA;
     # OppHistory <- FALSE;     OppHistSQL <- NA;
     # OppFieldHist <- FALSE;   OppFldHstSQL <- NA;
     # SaveResults <- FALSE;
     # LoadSF_Data <- FALSE;
     # SQLDirStrings <- c("sqlGlobal", "querySFDC")
     # 
     # 
     # here::here(SQLDirStrings,CampSQL)
     # 
     # 
     # 
     # 
     # 
     # 
     # 
