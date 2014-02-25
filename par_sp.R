#Parametros da espécie

landmarks <- list( 
         a=list(midline=c( "BR", "NON", "NLE", "IS", "MP", "PNS", "BA", "OPI"), 
						    side=c("EAM", "JP", "APET", "TS", "ZYGI", "TSP", "PT", "PZIG", "ZI", "PM", "PMA", "CP", "CAA", "CAP", "MT") ), 
				 
         z=list(midline=c("NLE", "NON", "BR", "LD", "BA"), 
				 		    side=c("AS", "PT", "PZIG", "ZYGS", "ZS", "FMN", "PMP", "NP")) 
                 )

common  <- c("BR", "NON", "NLE", "BA", "PT_E", "PZIG_E", "PT_D", "PZIG_D")
midline	<- c("NLE","NON","BR","LD","BA","IS","MP","PNS","OPI")
side    <- c("AS","PT","PZIG","ZYGS","ZS","FMN","PMP",
             "NP","EAM","JP","APET","TS","ZYGI","TSP",
             "ZI","PMA","PM","CP","CAA","CAP","MT")

left 	<-paste(side,"_E",sep="")
right	<-paste(side,"_D",sep="")

dists<-c(
"IS-PM",
"IS-NLE",
"IS-PNS",
"PM-ZS",
"PM-ZI",
"PM-MT",
"NLE-NON",
"NLE-ZS",
"NLE-ZI",
"NON-BR",
"NON-PNS",
"BR-PT",
"BR-APET",
"PT-APET",
"PT-BA",
"PT-EAM",
"PT-ZYGI",
"PT-TSP",
"ZS-ZI",
"ZI-MT",
"ZI-ZYGI",
"ZI-TSP",
"MT-PNS",
"PNS-APET",
"APET-BA",
"APET-TS",
"BA-EAM",
"EAM-ZYGI",
"ZYGI-TSP",
"LD-AS",
"BR-LD",
"OPI-LD",
"PT-AS",
"JP-AS",
"BA-OPI")
