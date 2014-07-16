## load files

#filename <-file.choose()

filename = "/Users/admin-local/Documents/R/challengeSncf/gares_apprentissage.csv";
garesRaw <- read.csv2(filename,sep = ";",header=TRUE);

filename = "/Users/admin-local/Documents/R/challengeSncf/gares_test.csv";
garesTestRaw <- read.csv2(filename,sep = ";",header=TRUE);

filename = "/Users/admin-local/Documents/R/challengeSncf/sncf-transilien-paniers-fraicheurs.csv";
paniers <- read.csv2(filename,sep = ";",header=TRUE);

filename = "/Users/admin-local/Documents/R/challengeSncf/sncf-lignes-par-gares-idf.csv";
lignesgares <- read.csv2(filename,sep = ";",header=TRUE);

## donnÃ©es apprentissage

nb = dim(garesRaw)[1];
gares = matrix(data = 0, nrow = nb, ncol = 2+dim(lignesgares)[2]-2+1);
for (i in 1:nb){
	gares[i,1] = garesRaw[i,2];
	if (garesRaw[i,3] == "moins de 300"){
		gares[i,2] = 1;
	} else if (garesRaw[i,3] == "entre 300 et 1000"){
		gares[i,2] = 2;
	} else if (garesRaw[i,3] == "entre 1000 et 5000"){
		gares[i,2] = 3;
	} else if (garesRaw[i,3] == "entre 5000 et 15000"){
		gares[i,2] = 4;
	} else if (garesRaw[i,3] == "plus de 15000"){
		gares[i,2] = 5;
	}

}

for (i in 1:dim(lignesgares)[1]){
	lignesgares[i,is.na(lignesgares[i,])] = 0;
	if (length(gares[which(gares[,1] == lignesgares[i,1]),1])>0){
		gares[which(gares[,1] == lignesgares[i,1]),3:(2+dim(lignesgares)[2]-2)] = unlist(lignesgares[i,c(3:dim(lignesgares)[2])]);
	}
}

for (j in 1:dim(paniers)[1]){
	if (length(gares[which(gares[,1] == paniers[j,2]),1])>0){
		gares[which(gares[,1] == paniers[j,2]),2+dim(lignesgares)[2]-2+1] = 1
	}
}


## donnÃ©es test


nb = dim(garesTestRaw)[1];
garesTest = matrix(data = 0, nrow = nb, ncol = 2+dim(lignesgares)[2]-2+1);
for (i in 1:nb){
	garesTest[i,1] = garesTestRaw[i,1];
}


for (i in 1:dim(lignesgares)[1]){
	lignesgares[i,is.na(lignesgares[i,])] = 0;
	if (length(garesTest[which(garesTest[,1] == lignesgares[i,1]),1])>0){
		garesTest[which(garesTest[,1] == lignesgares[i,1]),3:(2+dim(lignesgares)[2]-2)] = unlist(lignesgares[i,c(3:dim(lignesgares)[2])]);
	}
}

for (j in 1:dim(paniers)[1]){
	if (length(garesTest[which(garesTest[,1] == paniers[j,2]),1])>0){
		garesTest[which(garesTest[,1] == paniers[j,2]),2+dim(lignesgares)[2]-2+1] = 1
	}
}

## analyse en composante principale pour voir comment les donnÃ©es se rÃ©partissent les unes par rapport aux autres
pr = prcomp(gares[,3:22], scale = TRUE);
plot.new();
for (i in (1:nb)){
	plot(pr$rotation[,1]%*%gares[1,3:22],pr$rotation[,1]%*%gares[1,3:22]);
	#text(pr$rotation[,1]%*%gares[1,3:22],pr$rotation[,1]%*%gares[1,3:22],"i");
}

pr$rotation[,1]%*%gares[1,3:22]

biplot(prcomp(gares[,3:22], scale = TRUE))
