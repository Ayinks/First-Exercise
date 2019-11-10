install.packages(c("ggplot2", "dply", "gapminder"))
library("gapminder")
library("dplyr")
head(gapminder)
gap07<- filter(gapminder, year==2007)
filter(gapminder, country %in% c("France", "Turkey"))
filter(gapminder, year == 2007 & country != "Oceania")
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(medpop = median(pop))
library("ggplot2")
head(gap07)
ggplot(gap07, aes(x= gdpPercap, y = lifeExp)) + geom_point()
ggplot(gap07, aes(x= continent)) + geom_bar()
gap07med <- gap07 %>%
  group_by(continent) %>%
  summarise(pop = median(pop))
head(gap07med)
ggplot(gap07med, aes(x= continent, y = pop)) + geom_bar(stat = "identity")
gap_iceland <- filter(gapminder, country == "Iceland")
p <- ggplot(gap_iceland, aes(x= year, y = gdpPercap))
p + geom_point()
p + geom_line()
p  + geom_point() + geom_line()
p + 
  geom_point() + 
  geom_line()
  ggsave("iceland_gdp.png", height = 6, width = 8)
p2 <- ggplot(gap_iceland, aes(x= year, y = gdpPercap)) + geom_point() +geom_line() 
ggsave("iceland_gdp.png", plot = p2, height = 6, width = 8)
ggplot(gap07, aes(x= gdpPercap)) + geom_histogram(bins = 15)
ggplot(gap07, aes(x= gdpPercap)) + geom_density()
ggplot(gap07, aes(x= continent, y = lifeExp)) + geom_boxplot() + geom_point()
ggplot(gap07, aes(x= continent, y = lifeExp)) + geom_boxplot() + 
  geom_jitter(width = 0.3, alpha=0.2)
gap07<- filter(gapminder, year==2007)
ggplot(gap07, aes(x= gdpPercap, y = lifeExp, color = continent)) + geom_point()
ggplot(gap07, aes(x= gdpPercap, y = lifeExp, shape = continent, color = continent)) + geom_point()
ggplot(gap07, aes(x= gdpPercap, y = lifeExp, size = pop, color = continent)) + geom_point()
gappop <- gapminder %>%
  group_by(continent, year) %>%
  summarise(pop = mean(pop))
head(gappop)
ggplot(gappop, aes(x = year, y = pop) + geom_line()
library("ggplot2")      
ggplot(gappop, aes(x = year, y = pop, group = continent)) + geom_line()
ggplot(gappop, aes(x = year, y = pop, color = continent)) + geom_line()
ggplot(gap07, aes(x= gdpPercap, y = lifeExp)) + geom_point() +
  facet_grid(continent ~ year)
gaplife <- gapminder %>%
  group_by(continent, year) %>%
  summarise(lifeExp = mean(lifeExp))
ggplot(gaplife, aes(x= year, y = lifeExp)) + geom_line() +
  facet_grid(. ~ continent)
ggplot(gaplife, aes(x= year, y = lifeExp)) + geom_line() +
  facet_wrap( ~ continent)
ggplot(gap07, aes(x= gdpPercap, y = lifeExp)) + geom_point() +
  geom_smooth()
ggplot(gap07, aes(x= gdpPercap, y = lifeExp)) + geom_point() +
  geom_smooth(span = 0.2)
ggplot(gap07, aes(x= gdpPercap, y = lifeExp)) + geom_point() +
  geom_smooth(se = FALSE)
ggplot(gap07, aes(x= gdpPercap, y = lifeExp)) + geom_point() +
  geom_smooth(span = 0.9)
ggplot(gap07, aes(x= gdpPercap, y = lifeExp)) + geom_point() +
  geom_smooth(level = 0.2)
ggplot(gap07, aes(x= gdpPercap, y = lifeExp)) + geom_point() +
  geom_smooth(method = "lm")
ggplot(gap07, aes(x= gdpPercap, y = lifeExp)) + geom_point() +
  facet_wrap( ~ continent) +
  geom_smooth(aes(color = continent))
ggplot(gap07, aes(x= gdpPercap, y = lifeExp)) + geom_point() +
  scale_x_log10() +
  scale_y_continuous(limits = c(0, 95))
ggplot(gap07, aes(x= gdpPercap, y = lifeExp, color = continent)) + geom_point() +
  scale_x_log10() +
  scale_color_brewer(palette  = "Dark2")
ggplot(gap07, aes(x= gdpPercap, y = lifeExp, color = continent)) + geom_point() +
  scale_x_log10() +
  theme_grey()
ggplot(gap07, aes(x= gdpPercap, y = lifeExp, color = continent)) + geom_point() +
  scale_x_log10() +
  theme_bw()
ggplot(gap07, aes(x= gdpPercap, y = lifeExp, color = continent)) + geom_point() +
  scale_x_log10() +
  theme_dark()
ggplot(gap07, aes(x= gdpPercap, y = lifeExp, color = continent)) + geom_point() +
  scale_x_log10() +
  theme_void()

install.packages("ggthemes")
library("ggthemes")
ggplot(gap07, aes(x= gdpPercap, y = lifeExp, color = continent)) + geom_point() +
  scale_x_log10() +
  theme_solarized() +
  scale_color_solarized("blue")
ggplot(gap07, aes(x= gdpPercap, y = lifeExp, color = continent)) + geom_point() +
  scale_x_log10() +
  theme_base()
ggplot(gap07, aes(x= gdpPercap, y = lifeExp, color = continent)) + geom_point(size = 0.2) +
  scale_x_log10() +
  theme_light() +
  theme(legend.position = "bottom")
ggplot(gap07, aes(x= gdpPercap, y = lifeExp, color = continent)) + geom_point(size = 0.2) +
  scale_x_log10() +
  theme_light() +
  theme(legend.position = "none")
ggplot(gap07, aes(x= gdpPercap, y = lifeExp, color = continent)) + geom_point(size = 2) +
  scale_x_log10() +
  theme_light() +
  theme(legend.position = c(0.1, 0.85), axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14)) +
  labs(x = "Per Capital GDP", y = "Life Expectancy", 
       title = "2007 Life Expectancy and GDP", color = "continent") +
  ggsave("lifeExp_gdp_custom.png", width = 7, height = 7)