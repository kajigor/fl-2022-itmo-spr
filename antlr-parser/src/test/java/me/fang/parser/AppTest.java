package me.fang.parser;

import java.io.IOException;
import org.junit.jupiter.api.Test;

public class AppTest {
    @Test
    public void testMain() throws IOException {
        App.main(new String[]{"src/test/resources/test_data.json"});
    }
}
