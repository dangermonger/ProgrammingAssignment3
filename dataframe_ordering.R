df <- data.frame(item = sample(letters[1:9]),
                 year = sample(2010:2018),
                 count = rep(c(27, 28, 29), 3))
df
dforder <- df[order(df[3], df$item),] ##this will order the table by the count column then the item column
dforder <- order(df[3], df$item) ##this orders the dataframe without subsetting, returning row numbers of the ordered dataframe
dforder
df$item[dforder[1]]
df$item[dforder[length(dforder)]]

df[dforder, ] ##this is the subsetted dforder from above