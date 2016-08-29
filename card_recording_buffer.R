card_buffer <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/card_data_PCA_sans_recordings_2dec2015.csv")

buffer_pc1_mean = c(mean(card_buffer$PC1[card_buffer$Type=="Cactus.Wren"]),
                      mean(card_buffer$PC1[card_buffer$Type=="Texas"]),
                      mean(card_buffer$PC1[card_buffer$Type=="Bill.Williams"]),
                      mean(card_buffer$PC1[card_buffer$Type=="Portal"]))
buffer_pc1_sd = c(sd(card_buffer$PC1[card_buffer$Type=="Cactus.Wren"]),
                    sd(card_buffer$PC1[card_buffer$Type=="Texas"]),
                    sd(card_buffer$PC1[card_buffer$Type=="Bill.Williams"]),
                    sd(card_buffer$PC1[card_buffer$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
buffer_pc1_plot = barplot(buffer_pc1_mean,col=c("grey15","blue","green","purple"),
                            ylim=c(-1,2.5),ylab="pc1",xlab="Song Locality",names=songs,
                            main="100A_PCA")
error.bar(buffer_pc1_plot,buffer_pc1_mean,buffer_pc1_sd/sqrt(length(card_buffer$Type)/4))


t.test(card_buffer$PC1[card_buffer$Type=="Bill.Williams"],card_buffer$PC1[card_buffer$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 0.2736644

t.test(card_buffer$PC1[card_buffer$Type=="Bill.Williams"],card_buffer$PC1[card_buffer$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.4227854

t.test(card_buffer$PC1[card_buffer$Type=="Bill.Williams"],card_buffer$PC1[card_buffer$Type=="Portal"],paired=TRUE)$p.value
#p-value 9.337164e-06

t.test(card_buffer$PC1[card_buffer$Type=="Cactus.Wren"],card_buffer$PC1[card_buffer$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.6930996

t.test(card_buffer$PC1[card_buffer$Type=="Cactus.Wren"],card_buffer$PC1[card_buffer$Type=="Portal"],paired=TRUE)$p.value
#  p-value 3.778391e-07

t.test(card_buffer$PC1[card_buffer$Type=="Texas"],card_buffer$PC1[card_buffer$Type=="Portal"],paired=TRUE)$p.value
#p-value = 2.431602e-07