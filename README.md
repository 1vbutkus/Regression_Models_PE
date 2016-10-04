# Swirl lessons for R programming

This is a hub for swirl lessons that are dedicated for "practical econometric" lectures in Vilnius University.

Swirl teaches you R programming and data science interactively, at your own pace, and right in the R console.
See: http://swirlstats.com/

All lectures are based on swirl starting content. See: 
https://git hub.com/swirly/swirl_courses#swirl-courses

I just wanted to have some customization opportunity, thus, I duplicated the content.
Up till now there is only few changes, but I hope there will be more.

## Usage

I assume you already know how to install R on your machine. 
If not, please look at official swirl page that will guide you step by step: 
http://swirlstats.com/students.html 

For a first time run:
```
install.packages(c('swirl', 'base64enc'))
library("swirl")
install_course_github("1vbutkus", "Regression_Models_PE")
```

Next time to start a lesson just type:
```
library("swirl")
swirl()
```

After that all you need to do is to read messages and answer requested messages.






