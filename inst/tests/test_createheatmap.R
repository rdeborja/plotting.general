# test to create test heatmap
mtcars$car <- rownames(mtcars)
create.heatmap(
  data = mtcars,
  x = 'car',
  y = 'cyl',
  xlab = '',
  ylab = 'Number of Cylinders',
  fill = 'mpg',
  x.axis.label.rotate = 90
  );
