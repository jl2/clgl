#version 330 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 normal;
layout(location = 2) in vec4 color;

uniform mat4 projectionMatrix;

smooth out vec4 Color;

void main()
{
    vec4 L = normalize(vec4(vec3(10.0,10.0,10.0) - position.xyz, 1.0));
    Color = vec4(0.0125, 0.0125, 0.0125, 0.0125) + color * max(0.2, dot(L, inverse(projectionMatrix) * vec4(normal, 1.0)));
    // Color = color;
    gl_PointSize = 1.0;
    gl_Position = projectionMatrix * vec4(position, 1.0);
}
