#version 330 core
#extension GL_ARB_explicit_uniform_location : require

layout(location = 0) in vec3 point;
layout(location = 1) in vec3 normal;
layout(location = 2) in vec4 color;

uniform mat4 projectionMatrix;

smooth out vec4 Color;

void main()
{
    Color = color;
    gl_PointSize = 2.0;
    gl_Position = projectionMatrix * vec4(point, 1.0);
}
