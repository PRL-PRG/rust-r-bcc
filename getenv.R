base_list <- as.list(baseenv());

print(length(base_list));

res <- list();
for (i in 1:length(base_list)) {
    if (is.function(base_list[[i]])) {
        name <- attributes(base_list)[i];
        res <- append(res, list(base_list[[i]]));
    }
}


print(length(res));
saveRDS(res, file="temp/baseenv.dat", compress=FALSE, version=2)
