#version 330 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec4 color;

uniform mat4 transformationMatrix;

out vec4 Color;

void main()
{
    Color = color;
    gl_PointSize = 1.250;
    gl_Position = transformationMatrix * vec4(position, 1.0);
}
