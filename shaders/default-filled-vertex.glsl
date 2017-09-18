#version 330 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 normal;
layout(location = 2) in vec4 color;

uniform mat4 transformationMatrix;
uniform mat4 projectionMatrix;

out vec4 Color;

void main()
{
    vec4 transformedPosition = transformationMatrix * vec4(position.xyz, 1.0);
    vec4 L = vec4(normalize(vec3(transformedPosition.xyz - vec3(0.0,0.0,10.0))), 0.0);

    Color = vec4(0.0125, 0.25, 0.0125, 1.0) + color * max(0.2, dot(L, inverse(transformationMatrix) * vec4(normal, 0.0)));
    // Color = color;
    gl_PointSize = 1.0;
    gl_Position = projectionMatrix * transformationMatrix * vec4(position, 1.0);
}
