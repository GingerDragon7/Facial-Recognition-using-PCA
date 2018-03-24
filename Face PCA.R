
mydir = "  "

setwd(mydir)
library(pixmap)

######################################################################(a)
#Load the views P00A+000E+00, P00A+005E+10, P00A+005E-10, and P00A+010E+00 for all subjects. 
#Convert each photo to a vector; store the collection as a matrix where each row is a photo.
# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list = c(1:38)
view_list = c( 'P00A+000E+00', 'P00A+005E+10', 'P00A+005E-10', 'P00A+010E+00')
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)

pic_data = vector("list",length(pic_list)*length(view_list)) # preallocate an empty list
pic_data_pgm = vector("list",length(pic_list)*length(view_list)) # preallocate an empty list to store the pgm for debugging

# Preallocate matrix to store picture vectors, store sizes for computations
this_face = read.pnm(file = "CroppedYale/yaleB01/yaleB01_P00A+010E+00.pgm")
this_face_matrix = getChannels(this_face)
original_size = dim(this_face_matrix)
pic_vector_length = prod(original_size)
pic_mat = mat.or.vec(length(pic_list)*length(view_list),pic_vector_length)


for ( i in 1:length(pic_list) ){
	for ( j in 1:length(view_list) ){
		# compile the correct file name
		this_filename = sprintf("CroppedYale/%s/%s_%s.pgm", dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
		this_face = read.pnm(file = this_filename)
		this_face_matrix = getChannels(this_face)

		# store pgm as element of the list
		pic_data_pgm[[(i-1)*length(view_list)+j]] = this_face
		# store matrix as element of the list
		pic_data[[(i-1)*length(view_list)+j]] = this_face_matrix
		# make the face into a vector and include in the data matrix
		pic_mat[(i-1)*length(view_list)+j,] =  as.vector(this_face_matrix)
	}	
}

pic_mat_size = dim(pic_mat)
print(sprintf('The matrix of all faces has size %d by %d' , pic_mat_size[1] , pic_mat_size[2] ))

######################################################################a(b)
#Compute a "mean face," which is the average for each pixel across all of the faces.
#Subtract this off each of the faces. Display the mean face as a photo in the original size and save a copy as .png.

# Find the mean face vector
mean_face = colMeans(pic_mat)
# Now print it as a picture
mean_face_matrix = mean_face
dim(mean_face_matrix) = original_size
mean_face_pix = pixmapGrey(mean_face_matrix)
plot(mean_face_pix)

#Subtract off the mean face
pic_mat_centered = mat.or.vec(pic_mat_size[1],pic_mat_size[2])

for (i in 1:pic_mat_size[1]){
	pic_mat_centered[i,] = pic_mat[i,] - mean_face
}
 
#####################################################################(c)
#Use prcomp() to find the principal components of your image matrix. 
#Plot the number of components on the x-axis against the proportion of the variance explained on the y-axis.
pic_pca = prcomp(pic_mat_centered)

# make a vector to store the variances captured by the components
n_comp = length(pic_pca$x[,1])
pca_var = mat.or.vec(n_comp,1)
for (i in 1:n_comp){
	if (i==1){
		pca_var[i] = pic_pca$sdev[i]^2
	}else{
		pca_var[i] = pca_var[i-1] + pic_pca$sdev[i]^2
	}
}

pca_var = pca_var/pca_var[n_comp]*100
# now plot it against the number of components
plot(pca_var,ylim=c(-2,102),xlab="Number of Components",ylab="Percentage of Variance Captured")
# add a line at 100 to show max level
abline(h=100,col="red")

#######################################################################(d)
#Each principal component is a picture, which are called "eigenfaces". 
#Display the first 9 eigenfaces in a 3-by-3 grid.
eigenface_mat = vector()
ind = 9

# loop through first 9 eigenfaces
this_face_row = vector()

for (i in 1:ind){
	# Make the eigenface vector into a matrix
	this_eigenface = pic_pca$rotation[,i]
	dim(this_eigenface) = original_size
	this_face_row = cbind(this_face_row,this_eigenface)
	if ((i %% 3)==0){
		# make a new row
		eigenface_mat = rbind(eigenface_mat,this_face_row)
		# clear row vector
		this_face_row = vector()
	}
}
# Plot the eigenfaces
eigenface_pgm = pixmapGrey((eigenface_mat-min(eigenface_mat))/(max(eigenface_mat)-min(eigenface_mat)))
plot(eigenface_pgm)

#######################################################################(e)
#Use the eigenfaces to reconstruct yaleB05 P00A+010E+00.pgm.
#Starting with the mean face, add in one eigenface at a time until you reach 24 eigenfaces. 
#Save the results in a 5-by-5 grid. Again, starting with the mean face, add in five eigenfaces at a time until you reach
#120 eigenfaces. Save the results in a 5-by-5 grid. 
#How many faces do you feel like you need until you can recognize the person?

# Find the index of face yaleB01_P00A+010E+00.pgm
face_index = 20

# Make a function to produce a 5 x 5 matrix of faces
eigenface_add_by_face <- function(face_index, max_faces, by_faces, mean_face, pic_pca, original_size){
	# Initialize matrix for faces added in one eigenface at a time
	face_by_eig_mat = vector()
	face_by_eig_row = vector()
	face_by_eig_vector = mean_face
	# Store the temporary face as a matrix
	face_temp = face_by_eig_vector
	dim(face_temp) = original_size
	face_by_eig_row = cbind(face_by_eig_row,face_temp)
	
	# Now add in the eigenfaces
	for (i in 1:24){
		# Find the indices of the eigenfaces to include
		ind_include = seq((i-1)*by_faces+1,i*by_faces,1)
		# Add up the vector that is score[j] x eigenface[j]
		eigenface_add = mat.or.vec(length(mean_face),1)
		for (j in 1:length(ind_include)){
			ind_temp = ind_include[j]
			eigenface_add = eigenface_add + pic_pca$x[face_index,ind_temp]*pic_pca$rotation[,ind_temp]
		}
		face_by_eig_vector = face_by_eig_vector + eigenface_add
		# Transform this back to matrix and include
		face_temp = face_by_eig_vector
		dim(face_temp) = original_size
		face_by_eig_row = cbind(face_by_eig_row,face_temp)
		if ((i %% 5) == 4){
			# Start a new row
			face_by_eig_mat = rbind(face_by_eig_mat,face_by_eig_row)
			face_by_eig_row = vector()
		}
	}
	# Return the matrix of faces
	return(face_by_eig_mat) 
}

# Loop through the first 24 eigenfaces
max_faces = 24
by_faces = 1
face_by_1 = eigenface_add_by_face(face_index, max_faces, by_faces, mean_face, pic_pca, original_size)
face_by_1_pm = pixmapGrey(face_by_1)

# Plot results
plot(face_by_1_pm)


# Loop through the first 120 eigenfaces
max_faces = 120
by_faces = 5
face_by_5 = eigenface_add_by_face(face_index, max_faces, by_faces, mean_face, pic_pca, original_size)
face_by_5_pm = pixmapGrey(face_by_5)

# Plot results
plot(face_by_5_pm)


#######################################################################(f)
#Remove the pictures of subject 01 from your image matrix (there should be four pictures of him) and recenter the data. 
#Rerun prcomp() to get new principal components. Use these to reconstruct yaleB01 P00A+010E+00.pgm. 
#Do this by subtracting off the mean face and projecting the remaining image onto the principal components.
#Print the reconstructed image.
#oes it look like the original image? 

# Remove pictures 1 to 4 from matrix
pic_mat_mod = pic_mat[setdiff(1:pic_mat_size[1],1:4),]
pic_mat_mod_size = dim(pic_mat_mod)
# Recenter
mean_face_mod = colMeans(pic_mat_mod)
# Subtract off the mean face
pic_mat_mod_centered = mat.or.vec(pic_mat_mod_size[1],pic_mat_mod_size[2])
for (i in 1:pic_mat_mod_size[1]){
	pic_mat_mod_centered[i,] = pic_mat_mod[i,] - mean_face_mod
}
# Do the same thing for the query pic
query_pic = pic_mat[4,]
query_pic_centered = query_pic - mean_face_mod
# Do PCA
pic_pca_mod = prcomp(pic_mat_mod_centered)
# Get scores for query pic
num_comp_mod = length(pic_pca_mod$x[,1])
scores_query = mat.or.vec(num_comp_mod,1)
for (i in 1:num_comp_mod){
	loading_temp = pic_pca_mod$rotation[,i]
	scores_query[i] = loading_temp %*% query_pic_centered
}
# Use loadings to reconstruct picture
query_reconstruct = mean_face_mod
for (i in 1:num_comp_mod){
	query_reconstruct = query_reconstruct + scores_query[i]*pic_pca_mod$rotation[,i]
}
# Plot next to original for comparison
dim(query_reconstruct) = original_size
original_query = query_pic
dim(original_query) = original_size
side_by_side = cbind(original_query,query_reconstruct)
side_by_side_pm = pixmapGrey(side_by_side)
plot(side_by_side_pm)
filename = 'hw02_02f.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#################
# # End of Script
#################