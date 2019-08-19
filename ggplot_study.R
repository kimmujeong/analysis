library(ggplot2)
ggplot(mtcars, aes(x=wt, y=mpg))+geom_point()+labs(title="Automobile Data", x="Weight", y="Miles Per Gallon")
ggplot(mtcars, aes(x=wt, y=mpg))+geom_point(pch=17, color="blue", size=2)+geom_smooth(method="lm", color="red", linetype=2)+labs(title="Automobile Data", x="weight", y="Y")
mtcars$cyl<-factor(mtcars$cyl)
ggplot(mtcars, aes(x=wt, y=mpg, shape=cyl, color=cyl))+geom_point(size=3)+facet_grid(am~vs)

#집단호
library(car)
ggplot(Salaries, aes(x=salary, fill=rank))+geom_density(alpha=.3)
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank, shape=sex))+geom_point()
ggplot(Salaries, aes(x=rank, fill=sex))+geom_bar(position = "stack")
ggplot(Salaries, aes(x=rank, fill=sex))+geom_bar(position = "dodge")
ggplot(Salaries, aes(x=rank, fill=sex))+geom_bar(position = "fill")

#측면화, faceting
library(lattice)
ggplot(singer, aes(x=height))+geom_histogram()+facet_wrap(~voice.part, nrow=4)
ggplot(singer, aes(x=height, fill=voice.part))+geom_density()+facet_grid(voice.part~.)
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank, shape=rank))+geom_point()+facet_grid(.~sex)

#평활선
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, linetype=sex, shape=sex, color=sex))+geom_smooth(method=lm, formula = y~poly(x,2), se=FALSE, size=1)+geom_point()

#축 범위 수정
#continuous:눈금수정 discreate:수준배치
ggplot(Salaries, aes(x=rank, y=salary, fill=sex))+geom_boxplot()+
  scale_x_discrete(breaks=c("AsstProf", "AssocProf", "Prof"), labels=c("Assistant\nProfessor", "Associate\nProfessor","Full\nProfessor"))+
  scale_y_continuous(breaks=c(50000, 100000, 150000, 200000), labels = c("$50K","$100K","$150K","$200K"))

#범례수정
ggplot(Salaries, aes(x=rank, y=salary, fill=sex))+geom_boxplot()+
  scale_x_discrete(breaks=c("AsstProf", "AssocProf", "Prof"), labels=c("Assistant\nProfessor", "Associate\nProfessor","Full\nProfessor"))+
  scale_y_continuous(breaks=c(50000, 100000, 150000, 200000), labels = c("$50K","$100K","$150K","$200K"))+
  labs(title="title", x="x", y="y", fill="Gender")+theme(legend.position = c(.1,.8)) #.1:왼쪽모서리 .8:바닥모서리

#스케일
ggplot(mtcars, aes(x=wt, y=mpg, size=disp))+geom_point(shape=21, color="black", fill="cornsilk")
###색지정
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank))+scale_color_manual(values = c("orange","olivedrab","navy"))+geom_point(size=2)
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank))+scale_color_brewer(palette="Set1")+geom_point(size=2)

#사용가능한 색조합
library(RColorBrewer)
display.brewer.all()

#여러 그래프 그리기 grid.arrange
library(gridExtra)
p1<-ggplot(Salaries, aes(x=rank))+geom_bar()
p2<-ggplot(Salaries, aes(x=sex))+geom_bar()
p3<-ggplot(Salaries, aes(x=yrs.since.phd,y=salary))+geom_point()
grid.arrange(p1,p2,p3,ncol=3)

#그래프저장
ggsave(file="filename.png", plot=p1, width=5, height=4)