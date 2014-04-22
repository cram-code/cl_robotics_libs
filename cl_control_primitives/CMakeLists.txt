cmake_minimum_required(VERSION 2.8.3)
project(cl_control_primitives)

find_package(catkin REQUIRED COMPONENTS alexandria roslisp roslisp_utilities)

catkin_package()

include_directories(
  ${catkin_INCLUDE_DIRS}
)
