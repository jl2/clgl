#version 330 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec4 color;

uniform mat4 projectionMatrix;

out vec4 Color;

void main()
{
    Color = color;
    gl_PointSize = 1.0;
    gl_Position = projectionMatrix * vec4(position, 1.0);
}
