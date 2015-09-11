#' channel_analytic
#'
#' @description Extract many informative stats and object  from a set of tweet messages parsed as channel
#'
#' @param  channel_obj  Data.frame Dataset of tweets
#' @param  start_date   Character Date of   analisys start.
#' @param  end_date   Character Date of  analisys end.
#' @param  Ntop Integer indicate the maximum number for top statistics
#' @param  temporal_check Logical indicate if exist time consistency between dates and data
#' @param  Nmin Integer indicate the minimal data numerosity
#' @param  naming Character Indicate which naming framework is adopted.
#' @param  only_original_tweet Logical Taking into account only original. Default all tweets are considered.
#' @param  stopword Character stopword set to be use to calculate word frequency matrix. Default italian stopwords of R tm package.
#' @return Return a R list of object useful for channel analisys
#' @return channel_stat : channel summaries of following parameters.
#' @return *        N_tweets : Number of tweet within unique ID
#' @return *        N_retweets  (channel_stat):Number of retweet
#' @return *        N_native_tweets (channel_stat):Number of original tweet
#' @return *        N_hash  (channel_stat):Number of hashtags detected in channel
#' @return *        N_mention (channel_stat): Number of mention detected in channel
#' @return *        N_links  (channel_stat):Number of links detected in channel
#' @return *        Nuniq_authors (channel_stat):N of unique authors
#' @return *        Nuniq_hash (channel_stat): N of unique authors
#' @return *        Nuniq_mentions (channel_stat): N of unique authors
#' @return *        Nuniq_links (channel_stat):N of unique authors
#' @return *        Nfull_retweet (channel_stat):N of comprehensive retweet given by platorm
#' @return *        Nfull_retweet_missing (channel_stat):N tweet given without  platorm statistics
#' @return *        Nreplies (channel_stat):N of replies
#' @return *        Nfavor (channel_stat):N of cumulative favorites given by platorm
#' @return table_message :  Frequency data.frame of message
#' @return table_hash :  Frequency data.frame of hashtag
#' @return table_mentions :  Frequency data.frame of mentions
#' @return table_authors :  Frequency data.frame of authors
#' @return table_replies :  Frequency data.frame of replies
#' @return table_authors_retweeted  :  Frequency data.frame of authors retweeted
#' @return table_authors_retweeter :  Frequency data.frame of authors that is a retweeter
#' @return rank_authors_retweet :  Frequency data.frame of retweet by using platform data
#' @return rank_message_retweet :  Frequency data.frame of message by using platform data
#' @return top_message :  TopN messages in channel
#' @return top_authors :  TopN authors in channel
#' @return top_hash :  TopN hashtag
#' @return top_mentions :  TopN user mentioned
#' @return top_replies :  TopN user have made replies
#' @return top_authors_retweeted :  TopN user that have retweeted in channel
#' @return top_authors_retweeter :  TopN user that have made retweet in channel
#' @return topfull_authors_retweeted :  TopN author that have retweeted in platform
#' @return topfull_message_retweeted :  TopN message that have retweeted in platform
#' @return daily_stat :  Daily Temporal data of channel statistic data
#' @return authors_date :  DateTime authors activity in channel
#' @return links_date :  DateTime authors activity in channel
#' @return hash_date :  DateTime hashtag presence in channel
#' @return mentions_date :  DateTime mentions presence in channel
#' @return unique_message :  Unique message in channel
#' @return unique_authors :  Unique authors in channel
#' @return unique_hash :  Unique hashtag in channel
#' @return unique_mentions :  Unique mentions in channel
#' @return unique_authors_retweeted :  Unique retweeted user in channel
#' @return unique_authors_retweeter :  Unique retweeter user in channel
#' @return uniquefull_authors_retweeted :  Unique retweeted user in platform
#' @return uniquefull_message_retweeted :  Unique retweeted message in platform
#' @return graph_retweet_df :   Data used for retweet graph
#' @return graph_hash_df :  Data used for hashtag graph
#' @return graph_mentions_df :  Data for used mention graph
#' @return replies_df :  Data for replies
#' @return graph_retweet :   Retweet graph object as igraph R object
#' @return graph_mentions :   Mention graph object as igraph R object
#' @return authors_favorite :   rank of authors favorite
#' @return favorite_message_top :   TopN favorite message
#' @return channel_corpus :   Tm Corpus of messages without mentions and links and optionally without hashtag
#' @return word_freq_matr :   qdap wfm object Word frequency matrix.

#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  channel
#'
#'
#'
#' @export
#'
#'

channel_analytic=function(channel_obj,start_date, end_date,Ntop=11,temporal_check=FALSE,Nmin=25,naming="",only_original_tweet=FALSE,stopword = tm::stopwords("it")) {
  
  #####################################################################################
  
  require(qdap)
  require(qdapRegex)
  require(qdapTools)
  require(igraph)
  require(tm)
  
  #####################################################################################
  # Data checks
  rows=nrow(channel_obj)
  
  if (rows < Nmin) { stop("Channel with too few records.")};
  
  if (as.Date(start_date) > as.Date(end_date)) { stop("End Date older than Start date  ")};
  
  if ( naming == "DISIT") {
    
    channel_obj$text=channel_obj$message
    channel_obj$data=as.Date(channel_obj$publicationTime)
    channel_obj$screeName=channel_obj$twitterUser
    channel_obj$created=channel_obj$publicationTime
    channel_obj$ls_hash_full=channel_obj$hashtagsOnTwitter
    channel_obj$ls_links=channel_obj$links
    channel_obj$id=channel_obj$twitterId
    channel_obj$message<-NULL
    channel_obj$publicationTime<-NULL
    channel_obj$hashtagsOnTwitter<-NULL
    channel_obj$twitterId<-NULL
    channel_obj$links<-NULL
    
  }
  
  if ( naming == "twitter") {
    
    channel_obj$data=as.Date(channel_obj$created)
    
  }
  
  #####################################################################################
  # Temporal filter of channel
  if ( temporal_check==TRUE) {
    if (as.Date(start_date) < channel_obj$data[1]) { stop("Start Date of analisys not present." )};
    if (as.Date(end_date) > as.Date(tail(analitic_full_LIG_institution$data,1))) { stop("End Date of analisys not present." )};
    channel_obj=channel_obj[min(which(channel_obj$data==as.character(start_date))):max(which(channel_obj$data==as.character(end_date))),]
  }
   
  #####################################################################################
  # Create data.frames for other count statistics.
  
  ls_retweet=unlist(lapply(channel_obj$text,FUN=function(x) is.retweet(x)))
  
  if (only_original_tweet==TRUE) { channel_obj=channel_obj[which(ls_retweet==FALSE),]
                                   
                                 }
  
  if (only_original_tweet==TRUE) { ls_retweet=unlist(lapply(channel_obj$text,FUN=function(x) is.retweet(x)))
                                 }
  
  
   ####################################################################################
  # Create lists to be used for count statistics.
  
  ls_hash=lapply(channel_obj$text,FUN=function(x) rm_hash(x,extract=T))
  ls_tag=lapply(channel_obj$text,FUN=function(x) extract_mentions(x))
  ls_links=lapply(channel_obj$text,FUN=function(x) rm_url(x, extract=TRUE))

  ls_lenhash=unlist(lapply(channel_obj$text,FUN=function(x) length(rm_hash(x,extract=T)[[1]])))
  ls_lentag=unlist(lapply(channel_obj$text,FUN=function(x) length(extract_mentions(x)[[1]])))
  ls_words=unlist(lapply(channel_obj$text,FUN=function(x) word_count(x)))

  ####################################################################################
  # Extract replies and organize a frame
  
  replies_id=grep("^@",channel_obj$text)
  channel_obj$replies=NA
  channel_obj$replies[replies_id]=1
  ls_replies_df=data.frame(data=channel_obj$data,authors=channel_obj$screeName,replies=channel_obj$replies)
  
  ####################################################################################
  # Replies stats
  
  fullretweet_day=aggregate(channel_obj$retweetCount[which(!duplicated(channel_obj$text)==TRUE)],list(channel_obj$data[which(!duplicated(channel_obj$text)==TRUE)]),sum,na.rm = TRUE)
  names(fullretweet_day)=c("date","retweetCount")
  fullretweet_day$date=as.Date(fullretweet_day$date)
  
  fullreplies_day=aggregate(channel_obj$replies,list(channel_obj$data),sum,na.rm = TRUE)
  names(fullreplies_day)=c("date","repliesCount")
  fullreplies_day$date=as.Date(fullreplies_day$date)
  
  fullretweet_missing=length(which(is.na(channel_obj$retweetCount[which(!duplicated(channel_obj$text)==TRUE)])))
  fullretweet_channel_stat_sum=sum(channel_obj$retweetCount[which(!duplicated(channel_obj$text)==TRUE)],na.rm=T)
  replies_channel_stat_sum=length(replies_id)
  
  
  
    
 
  
  #######################################################################################
  # Create data.frame date,message and authors.
  
  ls_favorite_df=data.frame(data=channel_obj$data[which(!duplicated(channel_obj$text)==TRUE)],
                            message=channel_obj$text[which(!duplicated(channel_obj$text)==TRUE)],
                            authors=channel_obj$screeName[which(!duplicated(channel_obj$text)==TRUE)],
                            favoriteCount=channel_obj$favoriteCount[which(!duplicated(channel_obj$text)==TRUE)],
                            is.retweet=ls_retweet[which(!duplicated(channel_obj$text)==TRUE)])
  
  day_favorite=aggregate(ls_favorite_df$favoriteCount,list(ls_favorite_df$data),sum)
  names(day_favorite)<-c("date","N_favor")
  day_favorite$date=as.Date(day_favorite$date)
  
  
  ls_favorite_df=ls_favorite_df[order(-ls_favorite_df$favoriteCount),]
  
  
  rank_authors_favorite=aggregate(channel_obj$favoriteCount[which(!duplicated(channel_obj$text)==TRUE)],
                                  list(channel_obj$screeName[which(!duplicated(channel_obj$text)==TRUE)])
                                  ,sum)
  rank_authors_favorite=rank_authors_favorite[order(-rank_authors_favorite[,2]),]
  names(rank_authors_favorite)<-c("authors","favoriteCount")
  
  #########################################################################
  
  
  ls_message_df=data.frame(data=channel_obj$data[which(!duplicated(channel_obj$text)==TRUE)],
                          message=channel_obj$text[which(!duplicated(channel_obj$text)==TRUE)],
                          authors=channel_obj$screeName[which(!duplicated(channel_obj$text)==TRUE)],
                          retweetCount=channel_obj$retweetCount[which(!duplicated(channel_obj$text)==TRUE)],
                          is.retweet=ls_retweet[which(!duplicated(channel_obj$text)==TRUE)])
  
  rank_authors_retweet=aggregate(ls_message_df$retweetCount,list(ls_message_df$authors),sum)
  rank_authors=rank_authors_retweet[order(-rank_authors_retweet[,2]),]
  names(rank_authors_retweet)<-c("authors","retweetCount")
  
  
  rank_message_retweet=aggregate(ls_message_df$retweetCount,list(ls_message_df$message),sum)
  rank_message_retweet=rank_message_retweet[order(-rank_message_retweet[,2]),]
  names(rank_message_retweet)<-c("message","retweetCount")
  
  rank_authors_retweet=rank_authors_retweet[ order(-rank_authors_retweet[,2]), ]
  rank_message_retweet=rank_message_retweet[ order(-rank_message_retweet[,2]), ]
  
  ##########################################################################################################################################
  # retrieve other information from channel stack.
  
  rank_message_retweet$retweeted_authors=retweeted_users(rank_message_retweet$message)
  id_na_message_retweet=which(is.na(rank_message_retweet$retweeted_authors))
  not_retweet_with_authors=as.character(rank_message_retweet$message[id_na_message_retweet])
  
  for ( i in seq_along(not_retweet_with_authors)) {
    rank_message_retweet$retweeted_authors[id_na_message_retweet[i]]=as.character(channel_obj$screeName[min(which(channel_obj$text ==not_retweet_with_authors[i]))])
  }
  
  
  rank_message_retweet$data=NA
  
  
  for ( i in seq_along(rank_message_retweet$message)) {
    rank_message_retweet$data[i]=as.character(channel_obj$data[min(which((channel_obj$text %in%  rank_message_retweet$message[i] )==T))])
  }
  
  
  #######################################################################################
  # Create data.frame date,authors and is retweet.
  
  ls_authors_df=data.frame(data=channel_obj$data,authors=channel_obj$screeName,ls_retweet)
  names(ls_authors_df)=c("data","authors","retweet")
  
  
  #######################################################################################
  # Create data.frame date,retweeted_authors and authors.
  
  ls_retweeted_df=na.omit(data.frame(data=channel_obj$data,
                          retweeted_authors=sapply(channel_obj$text,FUN=retweeted_users),
                          authors=channel_obj$screeName))
  
  
  #####################################################################################
  # Create data.frame date and hashtag.
  
  
  ls_hash_long=list()
  for ( i in seq_along(ls_hash)) {ls_hash_long[[i]]=cbind(as.character(channel_obj$data[i]),unlist(ls_hash[[i]]),ls_retweet[i],as.character(channel_obj$screeName[i]))}
  ls_hash_df=as.data.frame(do.call(rbind,ls_hash_long))
  names(ls_hash_df)=c("data","hashtag","retweet","authors")
  
  #####################################################################################
  # Create data.frame date and mentions.
  
  ls_tag_long=list()
  for ( i in seq_along(ls_tag)){ls_tag_long[[i]]=cbind(as.character(channel_obj$data[i]),unlist(ls_tag[[i]]),ls_retweet[i],as.character(channel_obj$screeName[i]))}
  ls_tag_df=as.data.frame(do.call(rbind,ls_tag_long))
  names(ls_tag_df)=c("data","mention","retweet","authors")
  ls_tag_df$mention=gsub("^@","",ls_tag_df$mention)
  
  #####################################################################################
  # Create data.frame date and links.
  
  ls_links_long=list()
  for ( i in seq_along(ls_links)){ls_links_long[[i]]=cbind(as.character(channel_obj$data[i]),unlist(ls_links[[i]]),ls_retweet[i],channel_obj$screeName[i])}
  ls_links_df=as.data.frame(do.call(rbind,ls_links_long))
  names(ls_links_df)=c("data","links","retweet","authors")
  
  ###########################################################################################
  # Creating arrays date and elements
  
  authors_unique=na.omit(unique(ls_authors_df[,1:2]))
  links_unique=na.omit(unique(ls_links_df[,1:2]))
  hash_unique=na.omit(unique(ls_hash_df[,1:2]))
  tag_unique=na.omit(unique(ls_tag_df[,1:2]))
  
  authors_purged=na.omit(ls_authors_df)
  links_purged=na.omit(ls_links_df)
  hash_purged=na.omit(ls_hash_df)
  tag_purged=na.omit(ls_tag_df)
  
  #####################################################################################
  
  
  lenhash_df=data.frame(data=as.Date(channel_obj$data),lenhash=ls_lenhash)
  lenhash_df_day_mean=aggregate(lenhash_df$lenhash,list(lenhash_df$data),mean)
  names(lenhash_df_day_mean)=c("date","Nmean_hashtag")
  
  lentag_df=data.frame(data=as.Date(channel_obj$data),lentag=ls_lentag)
  lentag_df_day_mean=aggregate(lentag_df$lentag,list(lentag_df$data),mean)
  names(lentag_df_day_mean)=c("date","Nmean_mentions")
  
  lenwords_df=data.frame(data=as.Date(channel_obj$data),lenwords=ls_words)
  lenwords_df_day_mean=aggregate(lenwords_df$lenwords,list(lenwords_df$data),mean)
  names(lenwords_df_day_mean)=c("date","Nmean_words")
  
  ##############################################################################
  # retweet stats the ratio is ever ntive retweet/ native
  
  retweet_df=data.frame(data=channel_obj$data,is.retweet=ls_retweet)
  retweet_df_stats=as.data.frame.matrix(table(retweet_df$data,retweet_df$is.retweet))
  
  if(names(retweet_df_stats)[1] =="FALSE") {names(retweet_df_stats)[1] ="false"}
  if(names(retweet_df_stats)[1] =="TRUE") {names(retweet_df_stats)[1] ="true"}
  
  if (only_original_tweet==TRUE) { retweet_df_stats$true=rep(0,as.numeric(nrow(retweet_df_stats)))}
  
  retweet_df_stats$ratio=retweet_df_stats[,2]/retweet_df_stats[,1]
  retweet_df_stats$ratio[which(retweet_df_stats$ratio==Inf)]=NA
  retweet_stat=data.frame(date=as.Date(rownames(retweet_df_stats)),
                          native_tweets=retweet_df_stats[,1],
                          native_retweets=retweet_df_stats[,2],
                          retweet_ratio=retweet_df_stats$ratio)
  
  
  ########################################################################################
  # Creating daily stats
  
  lenauthorsunique_day=as.data.frame.array(table(authors_unique$data))
  lenauthorsunique_day_df=data.frame(date=as.Date(rownames(lenauthorsunique_day)),Nunique_authors=as.vector(lenauthorsunique_day[,1]))
  
  lenauthors_day=as.data.frame.array(table(authors_purged$data))
  lenauthors_day_df=data.frame(date=as.Date(rownames(lenauthors_day)),Nday_authors=as.vector(lenauthors_day[,1]))
  
  #####################
  
  lenlinksunique_day=as.data.frame.array(table(links_unique$data))
  lenlinksunique_day_df=data.frame(date=as.Date(rownames(lenlinksunique_day)),Nuniq_links=as.vector(lenlinksunique_day[,1]))
  
  lenlinks_day=as.data.frame.array(table(links_purged$data))
  lenlinks_day_df=data.frame(date=as.Date(rownames(lenlinks_day)),Nday_links=as.vector(lenlinks_day[,1]))
  
  #####################
  
  
  lenhashunique_day=as.data.frame.array(table(hash_unique$data))
  lenhashunique_day_df=data.frame(date=as.Date(rownames(lenhashunique_day)),Nuniq_hash=as.vector(lenhashunique_day[,1]))
  
  lenhash_day=as.data.frame.array(table(hash_purged$data))
  lenhash_day_df=data.frame(date=as.Date(rownames(lenhash_day)),Nday_hash=as.vector(lenhash_day[,1]))
  
  #####################
  
  lentagunique_day=as.data.frame.array(table(tag_unique$data))
  lentagunique_day_df=data.frame(date=as.Date(rownames(lentagunique_day)),Nuniq_mention=as.vector(lentagunique_day[,1]))
  
  lentag_day=as.data.frame.array(table(tag_purged$data))
  lentag_day_df=data.frame(date=as.Date(rownames(lentag_day)),Nday_mention=as.vector(lentag_day[,1]))
  
  
  #########################################################################
  # Create daily channel stats
  
  # Create a continuous data series
  
  ts_date=data.frame(date=seq.Date(as.Date(start_date),as.Date(end_date),1))
  daily_stat=merge(ts_date,retweet_stat,all.x=T)
  daily_stat=merge(daily_stat,lenauthors_day_df,all.x=T)
  daily_stat=merge(daily_stat,lenhash_day_df,all.x=T)
  daily_stat=merge(daily_stat,lentag_day_df,all.x=T)
  daily_stat=merge(daily_stat,lenlinks_day_df,all.x=T)
  daily_stat=merge(daily_stat,lenhash_df_day_mean,all.x=T)
  daily_stat=merge(daily_stat,lentag_df_day_mean,all.x=T)
  daily_stat=merge(daily_stat,lenwords_df_day_mean,all.x=T)
  daily_stat=merge(daily_stat,lenauthorsunique_day_df,all.x=T)
  daily_stat=merge(daily_stat,lenhashunique_day_df,all.x=T)
  daily_stat=merge(daily_stat,lentagunique_day_df,all.x=T)
  daily_stat=merge(daily_stat,lenlinksunique_day_df,all.x=T)
  daily_stat=merge(daily_stat,fullretweet_day,all.x=T)
  daily_stat=merge(daily_stat,fullreplies_day,all.x=T)
  daily_stat=merge(daily_stat,day_favorite,all.x=T)
  
  daily_stat$retweet_ratio=round(as.numeric(daily_stat$retweet_ratio),2)
  daily_stat$Nmean_hashtag=round(as.numeric(daily_stat$Nmean_hashtag),2)	
  daily_stat$Nmean_mentions=round(as.numeric(daily_stat$Nmean_mentions),2)	
  daily_stat$Nmean_words=round(as.numeric(daily_stat$Nmean_words),2)
  
  #################################################################################
  # Frequency analisys
  
  table_message=as.data.frame.array(sort(table(channel_obj$text),decreasing=T))
  
  table_message=data.frame(message=rownames(table_message),
                           Freq=as.vector(table_message))
  names(table_message)<-c("message","freq")
  
  rownames(table_message)<-NULL

  table_message$data=NA
  table_message$authors=NA
  
  
  ind=sapply(table_message$message,FUN=function(x) match(x,channel_obj$text))
  
  for ( i in 1:length(ind)) {
    table_message$data[i]=as.character(channel_obj$data[ind[i]])
    table_message$authors[i]=as.character(channel_obj$screeName[ind[i]])
  }
  
  table_message=na.omit(table_message)
  table_message$retweeted_authors=NA
  
  for ( i in 1:nrow(table_message)) {
    table_message$retweeted_authors[i]=retweeted_users(as.character(table_message$message[i]))
  }
  
  
  ##########################################################################
  if (only_original_tweet==FALSE) {
  table_authors_retweeted=as.data.frame.array(sort(table(ls_retweeted_df$retweeted_authors),decreasing=T))
  
  table_authors_retweeted=data.frame(authors=rownames(table_authors_retweeted),
                                     Freq=as.vector(table_authors_retweeted))
  names(table_authors_retweeted)<-c("authors_retweeted","freq")
  rownames(table_authors_retweeted)<-NULL
  }
  if (only_original_tweet==TRUE){
  table_authors_retweeted=data.frame(authors_retweeted=NA,freq=NA)
  };
  ##########################################################################
  if (only_original_tweet==FALSE) {
  
  table_authors_retweeter=as.data.frame.array(sort(table(ls_retweeted_df$authors),decreasing=T))
  table_authors_retweeter=data.frame(authors=rownames(table_authors_retweeter),
                                     Freq=as.vector(table_authors_retweeter))
  names(table_authors_retweeter)<-c("authors_retweeter","freq")
  rownames(table_authors_retweeter)<-NULL
  }
  if (only_original_tweet==TRUE){
  table_authors_retweeter=data.frame(authors_retweeter=NA,freq=NA)
  };
  ##########################################################################
  
  
  table_authors=as.data.frame.array(sort(table(ls_authors_df$authors),decreasing=T))
  
  table_authors=data.frame(authors=rownames(table_authors),
                           Freq=as.vector(table_authors))
  names(table_authors)<-c("authors","freq")
  rownames(table_authors)<-NULL
  
  ##########################################################################
  
  table_hash=as.data.frame.array(sort(table(ls_hash_df$hashtag),decreasing=T))
  
  table_hash=data.frame(hashtag=rownames(table_hash),
                        Freq=as.vector(table_hash))
  names(table_hash)<-c("hashtag","freq")
  rownames(table_hash)<-NULL
  
  ##########################################################################
  
  table_mentions=as.data.frame.array(sort(table(ls_tag_df$mention),decreasing=T))
  table_mentions=data.frame(users=rownames(table_mentions),
                            Freq=as.vector(table_mentions))
  names(table_mentions)<-c("mentions","freq")
  rownames(table_mentions)<-NULL
  
  ##########################################################################
  
  table_replies=as.data.frame.array(sort(table(ls_replies_df$authors),decreasing=T))
  table_replies=data.frame(users=rownames(table_replies),
                           Freq=as.vector(table_replies))
  names(table_replies)<-c("replies","freq")
  rownames(table_replies)<-NULL
  
  #########################################################################
  # Create full channel stats
  
  full_stat=data.frame(N_tweets=length(channel_obj$text),
                       N_retweets=sum(retweet_df_stats[,2],na.rm=T),
                       N_native_tweets=sum(retweet_df_stats[,1],na.rm=T),
                       N_hash=nrow(hash_purged),
                       N_mention=nrow(tag_purged),
                       N_links=nrow(links_purged),
                       Nuniq_authors=length(unique(ls_authors_df[,2])),
                       Nuniq_hash=length(unique(ls_hash_df[,2])),
                       Nuniq_mentions=length(unique(ls_tag_df[,2])),
                       Nuniq_links=length(unique(ls_links_df[,2])),
                       Nfull_retweet=fullretweet_channel_stat_sum,
                       Nfull_retweet_missing=fullretweet_missing,
                       Nreplies=replies_channel_stat_sum,
                       Nfavor=sum(ls_favorite_df$N_favor,na.rm=T)
                       
  )
  
  
  ############################################################################################################
  
  graph_mentions_df=na.omit(ls_tag_df)
  
  mat_men_graph=na.omit(data.frame(whopost=graph_mentions_df[,4],whomentioned=graph_mentions_df[,2]))
  men_graph = graph.edgelist(as.matrix(na.omit(mat_men_graph)))
  E(men_graph )$weight <- 1
  men_graph <- simplify(men_graph, remove.loops=FALSE)
  
  rt_graph= graph.edgelist(as.matrix(cbind(ls_retweeted_df[,3],ls_retweeted_df[,2])))
  E(rt_graph )$weight <- 1
  rt_graph <- simplify(rt_graph, remove.loops=FALSE)
  
  
  corpus=getCorpus(channel_obj$text)
  word_freq_matr=wfm(corpus,stopwords=stopword)
  
  ########################################################################################
  
  res=list(channel_stat=full_stat,
           table_message=table_message,
           table_hash=table_hash,
           table_mentions=table_mentions,
           table_authors=table_authors,
           table_replies=table_replies,
           table_authors_retweeted=table_authors_retweeted,
           table_authors_retweeter=table_authors_retweeter,
           rank_authors_retweet=rank_authors_retweet,
           rank_message_retweet=rank_message_retweet,
           top_message=table_message[1:Ntop,],
           top_authors=table_authors[1:Ntop,],
           top_hash=table_hash[1:Ntop,],
           top_mentions=table_mentions[1:Ntop,],
           top_replies=table_replies[1:Ntop,],
           top_authors_retweeted=table_authors_retweeted[1:Ntop,],
           top_authors_retweeter=table_authors_retweeter[1:Ntop,],
           topfull_authors_retweeted=rank_authors_retweet[1:Ntop,],
           topfull_message_retweeted=rank_message_retweet[1:Ntop,],
           daily_stat=daily_stat,
           authors_date=authors_purged,
           links_date=links_purged,
           hash_date=hash_purged,
           mentions_date=tag_purged,
           unique_message=unique(table_message[,1]),
           unique_authors=unique(table_authors[,1]),
           unique_hash=unique(table_hash[,1]),
           unique_mentions=unique(table_mentions[,1]),
           unique_authors_retweeted=unique(table_authors_retweeted[,1]),
           unique_authors_retweeter=unique(table_authors_retweeter[,1]),
           uniquefull_authors_retweeted=unique(rank_authors_retweet[,1]),
           uniquefull_message_retweeted=unique(rank_message_retweet[,1]),
           graph_retweet_df=ls_retweeted_df,
           graph_hash_df=na.omit(ls_hash_df),
           graph_mentions_df=na.omit(ls_tag_df),
           replies_df=ls_replies_df,
           graph_retweet=rt_graph,
           graph_mentions=men_graph,
           authors_favorite=rank_authors_favorite,
           favorite_message_top=head(ls_favorite_df,Ntop),
           channel_corpus=corpus,
           word_freq_matr=word_freq_matr
  )
  
  return(res)
}
