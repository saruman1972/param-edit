package com.allinfinance.tools.param.util;

public class EnumUtil {
    public static Object valueOf(Class<?> enumType, String name) {
        return Enum.valueOf((Class<? extends Enum>)enumType, name);
    }
}





















