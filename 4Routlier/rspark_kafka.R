
#kafka 生产者和消费者，rkafka可以消费
#http://itfish.net/article/54882.html
#准备工作，启动kafka和spark
############################## 
cd e:\ADwork\books\Rmarkdown\introRslide\4Routlier\
d:\python27\scripts\jupyter notebook

'''
cd D:\users\zookeeper-3.4.8
.\bin\zkServer

cd D:\users\kafka_2.11-0.10.0.0
.\bin\windows\kafka-server-start.bat .\config\server.properties

kafka-topics.bat --create --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic test
kafka-console-producer.bat --broker-list localhost:9092 --topic test
kafka-console-consumer.bat --zookeeper localhost:2181   --topic test
# rkafka也可消费
library(rkafka)
consumer1=rkafka.createConsumer("127.0.0.1:2181","test2",groupId="test-consumer-group")
for (i in 1:10) {
    print(i)
    print(rkafka.read(consumer1))
    Sys.sleep(2)
}
rkafka.closeConsumer(consumer1)


'''

#http://www.aboutyun.com/thread-9580-1-1.html
#spark kafka wordcount 消息生成与消费
'''
cd D:\soft\fenxi\spark\spark-1.6.1-bin-hadoop2.6
bin\run-example org.apache.spark.examples.streaming.KafkaWordCountProducer localhost:9092 test 1 5
bin\run-example org.apache.spark.examples.streaming.KafkaWordCount localhost:2181 test-consumer-group test 1
# rkafka也可消费
consumer2=rkafka.createConsumer("127.0.0.1:2181","test2",groupId="test-consumer-group")
print(rkafka.read(consumer2))
rkafka.closeConsumer(consumer2)

''' 

# 示例流程：
# ... | spark.streaming | spark.producer | rkafka.cosumer | 
#   rmodel.apply | rkafka.producer | spark.consumer | ...
getwd()
setwd('./4Routlier/')


# train model cluster
data(iris)
df_x=iris
#df_x 标准化
head(df_x)
xname=c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
yname=c('Species')
cl_model=kmeans(df_x[,xname],3)
plot(df_x[,1:2], col = cl_model$cluster)
plot(df_x[,3:4], col = cl_model$cluster)

getcl <- function(x,cl){
  c=cl$centers;nc=nrow(c)
  xnew=c()
  for (i in 1:nc) xnew=rbind(xnew,x)
  clsum=rowSums((c-xnew)^2)
  return(which.min(clsum) )
}
  
i=2
getcl(df_x[i,xname],cl_model)

# train model lm
#lm_model=lm(y~x1+x2+x3+x4+x5-1,df_x)
lm_model=lm(Petal.Width~Petal.Length-1,df_x)
plot(Petal.Width~Petal.Length,df_x) 
lines(lm_model$fitted.values~df_x$Petal.Length,col='red')

save(cl_model,lm_model,getcl,file='test.model')

x=df_x[1,xname]
predict(lm_model,as.data.frame(x))


#test load data
setwd('./4Routlier/')
df_x=iris
load('test.model')
getcl(df_x[1,1:4],cl_model)
predict(lm_model,df_x[1,])


# load and predict
library(rkafka)
load('test.model')

prod1=rkafka.createProducer("127.0.0.1:9092")
consumer=rkafka.createConsumer("127.0.0.1:2181","test",groupId="test-consumer-group")

# while (true | x>3)
xname=c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
yname=c('Species')
clname=c('cl1','cl2','cl3')
nstop=1
while (nstop<9) {
  instr=rkafka.read(consumer)
  print(instr)
  x=as.numeric(unlist(strsplit(instr,' ') ) )
  names(x)=c(xname,yname)
  #
  outcl=getcl(x[1:4],cl_model)
  outlm=predict(lm_model,as.data.frame(t(x)))
  #
  outstr=paste(instr,'|',outcl,'|',outlm )
  rkafka.send(prod1,"test2","127.0.0.1:9092",outstr)
  nstop=x[1]
}


rkafka.closeConsumer(consumer)
rkafka.closeProducer(prod1)





