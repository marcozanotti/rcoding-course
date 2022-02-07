# R Coding ----

# Lecture 3: Tidyverse Visualization --------------------------------------
# 2021/2022
# Marco Zanotti

# Goals:
# - Ggplot2
# - Ggplot2 Extensions
# - Plotly & ggplotly



# Tidyverse ---------------------------------------------------------------

# https://www.tidyverse.org/
# https://www.tidyverse.org/packages/
# https://tidyverse.tidyverse.org/index.html

# The tidyverse is an opinionated collection of R packages designed for 
# data science. All packages share an underlying design philosophy, 
# grammar, and data structures.

library(tidyverse)



# Ggplot2 -----------------------------------------------------------------

# https://ggplot2.tidyverse.org/
# https://exts.ggplot2.tidyverse.org/index.html

# ggplot2 is a system for declaratively creating graphics, based on 
# The Grammar of Graphics. You provide the data, tell ggplot2 how to map 
# variables to aesthetics, what graphical primitives to use, and it takes 
# care of the details.

# R has several systems for making graphs, but ggplot2 is one of the most 
# elegant and most versatile. ggplot2 implements the grammar of graphics, 
# a coherent system for describing and building graphs. With ggplot2, you 
# can do more faster by learning one system and applying it in many places.
# Throughout this section we will dive into ggplot2 using the mpg dataframe.

library(ggplot2)

# MPG dataset
mpg
str(mpg)
?mpg

# Diamonds dataset
diamonds
str(diamonds)
?diamonds


# * Initialization --------------------------------------------------------

# A ggplot2 chart is created with two main functions:
#   1. ggplot() initializes a ggplot object. It can be used to declare the input 
#      data frame for a graphic and to specify the set of plot aesthetics 
#      intended to be common throughout all subsequent layers unless 
#      specifically overridden
#   2. geom_...() represents the layers of the plot, that is the type of chart
#      one wants to draw (that is points, lines, bars, etc.)
# Moreover, in order to draw the plot, it is necessary to provide the data and
# to map the desired aesthetics.

# initializes the pane to the plot
ggplot() 

# adds a desired layer
ggplot() + 
  geom_point()

# provides the dataframe 
ggplot(data = mpg)

# maps the variables
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# With ggplot2, you begin a plot with the function ggplot(). ggplot() 
# creates a coordinate system that you can add layers to. The first argument 
# of ggplot() is the dataset to use in the graph. 
 
# You complete your graph by adding one or more layers to ggplot(). 
# The function geom_point() adds a layer of points to your plot, which 
# creates a scatterplot. ggplot2 comes with many geom functions that each 
# add a different type of layer to a plot. You’ll learn a whole bunch of 
# them throughout this chapter.

# Each geom function in ggplot2 takes a mapping argument. This defines how 
# variables in your dataset are mapped to visual properties. The mapping 
# argument is always paired with aes(), and the x and y arguments of aes() 
# specify which variables to map to the x and y axes. ggplot2 looks for 
# the mapped variables in the data argument, in this case, mpg.

# Graphing Template
# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))


# * Aesthetics Mappings ---------------------------------------------------

# You can add a third variable, like class, to a two dimensional scatterplot 
# by mapping it to an aesthetic. An aesthetic is a visual property of the 
# objects in your plot. Aesthetics include things like the size, the shape, 
# or the color of your points. You can display a point 
# in different ways by changing the values of its aesthetic properties. 
# Since we already use the word “value” to describe data, let’s use the word
# “level” to describe aesthetic properties. Here we change the levels of a 
# point’s size, shape, and color to make the point small, triangular, or blue.

mpg$class %>% table()

# coloring points by class using color aesthetic 
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, col = class))

# To map an aesthetic to a variable, associate the name of the aesthetic to 
# the name of the variable inside aes(). ggplot2 will automatically assign 
# a unique level of the aesthetic (here a unique color) to each unique value 
# of the variable, a process known as scaling. ggplot2 will also add a legend 
# that explains which levels correspond to which values.

# changing the size of points by class using size aesthetic 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# changing the transparency of points by class using alpha aesthetic
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# changing the shape of points by class using shape aesthetic
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# For each aesthetic, you use aes() to associate the name of the aesthetic 
# with a variable to display. The aes() function gathers together each of 
# the aesthetic mappings used by a layer and passes them to the layer’s 
# mapping argument. The syntax highlights a useful insight about x and y: 
# the x and y locations of a point are themselves aesthetics, visual 
# properties that you can map to variables to display information about 
# the data.

# Once you map an aesthetic, ggplot2 takes care of the rest. It selects 
# a reasonable scale to use with the aesthetic, and it constructs a legend 
# that explains the mapping between levels and values. For x and y aesthetics, 
# ggplot2 does not create a legend, but it creates an axis line with tick 
# marks and a label. The axis line acts as a legend; it explains the mapping
# between locations and values.

# You can also set the aesthetic properties of your geom manually.
# Here, the color doesn’t convey information about a variable, but 
# only changes the appearance of the plot. To set an aesthetic manually, 
# set the aesthetic by name as an argument of your geom function; i.e. 
# it goes outside of aes(). You’ll need to pick a level that makes 
# sense for that aesthetic: the name of a color as a character string; 
# the size of a point in mm; the shape of a point as a number.

# manual aesthetic
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), col = "blue")


# * Geometries ------------------------------------------------------------

# Each plot uses a different visual object to represent the data. 
# In ggplot2 syntax, we say that they use different geoms.
# A geom is the geometrical object that a plot uses to represent data. 
# People often describe plots by the type of geom that the plot uses. 
# For example, bar charts use bar geoms, line charts use line geoms, 
# boxplots use boxplot geoms, and so on. Scatterplots break the trend; 
# they use the point geom. As we see above, you can use different geoms 
# to plot the same data. To change the geom in your plot, change the 
# geom function that you add to ggplot().

# points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# smooth line
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# Every geom function in ggplot2 takes a mapping argument. However, 
# not every aesthetic works with every geom. You could set the shape of 
# a point, but you couldn’t set the “shape” of a line. On the other hand, 
# you could set the linetype of a line. geom_smooth() will draw a 
# different line, with a different linetype, for each unique value of 
# the variable that you map to linetype.

# does not work properly
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, shape = drv))

# changing line type by drv
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

# It is also possible to use more than one aesthetic into a single geom.

# changing line type and color by drv
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, col = drv))

# Many geoms, like geom_smooth(), use a single geometric object to display 
# multiple rows of data. For these geoms, you can set the group aesthetic 
# to a categorical variable to draw multiple objects. ggplot2 will draw 
# a separate object for each unique value of the grouping variable. 
# In practice, ggplot2 will automatically group the data for these geoms 
# whenever you map an aesthetic to a discrete variable (as in the linetype 
# example). It is convenient to rely on this feature because the group 
# aesthetic by itself does not add a legend or distinguishing features 
# to the geoms.

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# grouping by drv
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

# One may want to display multiple geoms in the same plot. To do this simply 
# add multiple geom functions to ggplot().
  
# point + smooth
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# This, however, introduces some duplication in our code. Imagine if you 
# wanted to change the y-axis to display cty instead of hwy. You’d need to 
# change the variable in two places, and you might forget to update one. 
# You can avoid this type of repetition by passing a set of mappings to 
# ggplot(). ggplot2 will treat these mappings as global mappings that 
# apply to each geom in the graph.

# global mapping, point + smooth
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# If you place mappings in a geom function, ggplot2 will treat them as local 
# mappings for the layer. It will use these mappings to extend or overwrite 
# the global mappings for that layer only. This makes it possible to display
# different aesthetics in different layers.

# global mapping x & y, local mapping col, point + smooth
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

# You can use the same idea to specify different data for each layer. 
# Here, our smooth line displays just a subset of the mpg dataset, 
# the subcompact cars. The local data argument in geom_smooth() overrides 
# the global data argument in ggplot() for that layer only.

# global data into point + local data into smooth
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)


# * Statistical Transformations -------------------------------------------

# Consider a basic bar chart, as drawn with geom_bar().

# geom_bar with default stat_count
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

# On the x-axis, the chart displays cut, a variable from diamonds. 
# On the y-axis, it displays count, but count is not a variable in diamonds!
# Where does count come from? Many graphs, like scatterplots, plot the 
# raw values of your dataset. Other graphs, like bar charts, calculate 
# new values to plot:
#   - bar charts, histograms, and frequency polygons bin your data and then 
#     plot bin counts, the number of points that fall in each bin
#   - smoothers fit a model to your data and then plot predictions from the model
#   - boxplots compute a robust summary of the distribution and then display 
#     a specially formatted box
# The algorithm used to calculate new values for a graph is called a stat, 
# short for statistical transformation. The figure below describes how this 
# process works with geom_bar().

# You can learn which stat a geom uses by inspecting the default value for 
# the stat argument. For example, ?geom_bar shows that the default value for 
# stat is “count”, which means that geom_bar() uses stat_count().
# You can generally use geoms and stats interchangeably. For example, you 
# can recreate the previous plot using stat_count() instead of geom_bar().
# This works because every geom has a default stat; and every stat has a 
# default geom. This means that you can typically use geoms without worrying 
# about the underlying statistical transformation.
 
# stat_count with default geom_bar 
ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

# There are three reasons you might need to use a stat explicitly:
#   1. You might want to override the default stat. In the code below, 
#      I change the stat of geom_bar() from count (the default) to identity. 
#      This lets me map the height of the bars to the raw values of a y 
#      variable
#   2. You might want to override the default mapping from transformed 
#      variables to aesthetics. For example, you might want to display a 
#      bar chart of proportion, rather than count
#   3. You might want to draw greater attention to the statistical 
#      transformation in your code. For example, you might use 
#      stat_summary(), which summarises the y values for each unique x 
#      value, to draw attention to the summary that you’re computing

# changing count to proportions
ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

# computing specific stats
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )


# * Position Adjustments --------------------------------------------------

# There’s one more piece of magic associated with bar charts. You can colour 
# a bar chart using either the colour aesthetic, or, more usefully, fill.
  
# color by cut
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))

# fill by cut
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))
  
# Note what happens if you map the fill aesthetic to another variable, 
# like clarity: the bars are automatically stacked. Each colored rectangle 
# represents a combination of cut and clarity.

# automatic stacked bars with fill by another variable (clarity)
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

# The stacking is performed automatically by the position adjustment 
# specified by the position argument. If you don’t want a stacked bar 
# chart, you can use one of three other options: "identity", "dodge" or 
# "fill".
#   - position = "identity" will place each object exactly where it falls 
#     in the context of the graph. This is not very useful for bars, because 
#     it overlaps them. To see that overlapping we either need to make the 
#     bars slightly transparent by setting alpha to a small value, or 
#     completely transparent by setting fill = NA
#   - position = "fill" works like stacking, but makes each set of stacked 
#     bars the same height. This makes it easier to compare proportions 
#     across groups
#   - position = "dodge" places overlapping objects directly beside 
#     one another. This makes it easier to compare individual values
      
# position identity makes overlapping bars   
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

# position fill creates proportional stacked bars
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

# position dodge creates bars next to each other
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

# There’s one other type of adjustment that’s not useful for bar charts, 
# but it can be very useful for scatterplots.
# The values in scatterplots are usually rounded so the points appear on 
# a grid and many points overlap each other. This problem is known as 
# overplotting. This arrangement makes it hard to see where the mass of 
# the data is. You can avoid this gridding by setting the position 
# adjustment to “jitter”. position = "jitter" adds a small amount of 
# random noise to each point. This spreads the points out because no two 
# points are likely to receive the same amount of random noise.
# Adding randomness seems like a strange way to improve your plot, but while 
# it makes your graph less accurate at small scales, it makes your graph more 
# revealing at large scales. Because this is such a useful operation, ggplot2 
# comes with a shorthand for geom_point(position = "jitter"): geom_jitter().

# scatterplot with overplotted points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# scatterplot with noise using position jitter or geom_jitter
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
ggplot(data = mpg) + 
  geom_jitter(mapping = aes(x = displ, y = hwy))


# * Coordinate Systems ----------------------------------------------------

# Coordinate systems are probably the most complicated part of ggplot2. 
# The default coordinate system is the Cartesian coordinate system where 
# the x and y positions act independently to determine the location of 
# each point. There are a number of other coordinate systems that are 
# occasionally helpful.
#   - coord_flip() switches the x and y axes. This is useful (for example), 
#     if you want horizontal boxplots. It’s also useful for long labels: 
#     it’s hard to get them to fit without overlapping on the x-axis
#   - coord_quickmap() sets the aspect ratio correctly for maps. This is very 
#     important if you’re plotting spatial data with ggplot2
#   - coord_polar() uses polar coordinates. Polar coordinates reveal an 
#     interesting connection between a bar chart and a Coxcomb chart

# boxplot
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
# boxplot with flipped x and y axes
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

# bar with flipped x and y axes
bar + coord_flip()
# bar with polar axes
bar + coord_polar()


# * Facets ----------------------------------------------------------------

# One way to add additional variables is with aesthetics. Another way, 
# particularly useful for categorical variables, is to split your plot 
# into facets, subplots that each display one subset of the data.
# To facet your plot by a single variable, use facet_wrap(). The first 
# argument of facet_wrap() should be a formula, which you create with ~ 
# followed by a variable name (here “formula” is the name of a data 
# R structure in R, not a synonym for “equation”). The variable that you 
# pass to facet_wrap() should be discrete.

# scatterplot faceted by class
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# To facet your plot on the combination of two variables, add facet_grid() 
# to your plot call. The first argument of facet_grid() is also a formula. 
# This time the formula should contain two variable names separated by a ~.

# scatterplot faceted by drv (to rows) and cyl (to columns)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

# If you prefer to not facet in the rows or columns dimension, use a . 
# instead of a variable name, e.g. + facet_grid(. ~ cyl).

# scatterplot faceted by cyl (to rows)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(cyl ~ .)

# scatterplot faceted by cyl (to columns)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)


# * Other Useful Graphical Designs ----------------------------------------

# Scale
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_jitter() +
  geom_violin(mapping = aes(fill = drv), alpha = .5) +
  geom_hline(
    yintercept = mpg %>% group_by(drv) %>% summarise(means = mean(hwy)) %>% pull(means), 
    linetype = 2, 
    col = c("red", "green", "blue")
  ) + 
  scale_fill_manual(values = c("red", "green", "blue"))

# Labs
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_jitter() +
  geom_violin(mapping = aes(fill = drv), alpha = .5) +
  geom_hline(
    yintercept = mpg %>% group_by(drv) %>% summarise(means = mean(hwy)) %>% pull(means), 
    linetype = 2, 
    col = c("red", "green", "blue")
  ) + 
  scale_fill_manual(values = c("red", "green", "blue")) +
  labs(
    title = "Boxplots", 
    subtitle = "colored by drv", 
    caption = "Fig. 1",
    x = "Classes", 
    y = "HMY", 
    fill = "DRV Types" # legend title
  )

# Themes
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_jitter() +
  geom_violin(mapping = aes(fill = drv), alpha = .5) +
  geom_hline(
    yintercept = mpg %>% group_by(drv) %>% summarise(means = mean(hwy)) %>% pull(means), 
    linetype = 2, 
    col = c("red", "green", "blue")
  ) + 
  scale_fill_manual(values = c("red", "green", "blue")) +
  labs(
    title = "Boxplots", 
    subtitle = "colored by drv", 
    caption = "Fig. 1",
    x = "Classes", 
    y = "HMY", 
    fill = "DRV Types" # legend title
  ) +
  theme_dark() +
  # theme_bw() + 
  # theme_minimal() + 
  theme(legend.position = "none") + 
  NULL


# * The Grammar of Graphics Template --------------------------------------

# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(
#     mapping = aes(<MAPPINGS>),
#     stat = <STAT>, 
#     position = <POSITION>
#   ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>

# Our new template takes seven parameters, the bracketed words that appear
# in the template. In practice, you rarely need to supply all seven
# parameters to make a graph because ggplot2 will provide useful defaults 
# for everything except the data, the mappings, and the geom function.
# 
# The seven parameters in the template compose the grammar of graphics, 
# a formal system for building plots. The grammar of graphics is based 
# on the insight that you can uniquely describe any plot as a combination 
# of a dataset, a geom, a set of mappings, a stat, a position adjustment, 
# a coordinate system, and a faceting scheme.


# Ggplot2 Extensions ------------------------------------------------------

# https://exts.ggplot2.tidyverse.org/index.html


# * Patchwork -------------------------------------------------------------

# Patchwork allows to combine different ggplot2 objects

library(patchwork)

p1 <- ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()
p2 <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

p1 | p2 # patchwork sintax


wrap_plots(p1, p2, guides = "collect") &
  guides(colour = guide_legend(nrow = 1)) &
  theme(legend.position = "top")

p3 <- ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_jitter() +
  geom_violin(mapping = aes(fill = drv), alpha = .5) +
  geom_hline(
    yintercept = mpg %>% group_by(drv) %>% summarise(means = mean(hwy)) %>% pull(means), 
    linetype = 2, 
    col = c("red", "green", "blue")
  ) + 
  scale_fill_manual(values = c("red", "green", "blue")) +
  labs(
    title = "Boxplots", 
    subtitle = "colored by drv", 
    caption = "Fig. 1",
    x = "Classes", 
    y = "HMY", 
    fill = "DRV Types"
  )

(p1 | p2) / p3 # patchwork sintax



# Plotly ------------------------------------------------------------------

# https://plotly.com/graphing-libraries/
# https://plotly.com/r/

# Plotly is a library that offers interactive charts and maps for Python, 
# R, Julia, Javascript, ggplot2, F#, MATLAB®, and Dash.

install.packages("plotly")
library(plotly)

# Plotly is a huge library, like ggplot2. I want to focus here on how to 
# convert ggplot2 ogbjects into plotly interactive charts.
# With ggplotly() by Plotly, you can convert your ggplot2 figures into 
# interactive ones powered by plotly.js, ready for embedding into applications.

gp <- mpg %>% 
  ggplot(mapping = aes(x = displ, y = hwy, col = cyl)) + 
  geom_jitter() + 
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~ year, nrow = 2, scales = "free_y") +
  labs(
    title = "Linear Regression Model", 
    subtitle = "1999 - 2008", 
    caption = "Fig. 1 The linear regression relationship between hmy and displ over years.",
    x = "Engine Displacement", 
    y = "Highway Miles per Gallon", 
    col = "Cylinders"
  ) +
  theme_minimal() + 
  theme(legend.position = "bottom")

# ggplot2 chart
gp

# plotly chart 
# not exactly the same because not all ggplot2 functionalities have a 
# plotly counterpart
ggplotly(gp)

