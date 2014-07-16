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

## données apprentissage

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


## données test


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

## analyse en composante principale pour voir comment les données se répartissent les unes par rapport aux autres
pr = prcomp(gares[,3:22], scale = TRUE);
plot.new();
for (i in (1:nb)){
	plot(pr$rotation[,1]%*%gares[1,3:22],pr$rotation[,1]%*%gares[1,3:22]);
	#text(pr$rotation[,1]%*%gares[1,3:22],pr$rotation[,1]%*%gares[1,3:22],"i");
}

pr$rotation[,1]%*%gares[1,3:22]

biplot(prcomp(gares[,3:22], scale = TRUE))


## linear discriminant analysis
library(MASS)
x <-lda(gares[,3:22], gares[,2])
y <- predict(x, garesTest[,3:22])$class

garesTest[,2] = y

res = matrix(data = 0, nrow = dim(garesTest)[1], ncol = 2)
for (i in 1:dim(garesTest)[1]){
	res[i,1] = garesTest[i,1];
	if (garesTest[i,2] == 1){
		res[i,2] = "moins de 300";
	} else if (garesTest[i,2] == 2){
		res[i,2] = "entre 300 et 1000";
	} else if (garesTest[i,2] == 3){
		res[i,2] = "entre 1000 et 5000";
	} else if (garesTest[i,2] == 4){
		res[i,2] = "entre 5000 et 15000";
	} else if (garesTest[i,2] == 5){
		res[i,2] = "plus de 15000";
	}
}
# attention changer séparateur ";", et nom des colonnes Code UIC; Classe estimée
write.csv(res, file = "/Users/admin-local/Documents/R/challengeSncf/prediction.csv",row.names=FALSE, col.names =FALSE, sep";") 
