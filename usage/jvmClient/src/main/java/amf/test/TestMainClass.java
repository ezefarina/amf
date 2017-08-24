package amf.test;
import amf.client.HintHelper;
import amf.client.JvmClient;
import amf.client.JvmGenerator;
import amf.client.VendorHelper;
import amf.model.*;
import com.sun.istack.internal.NotNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public class TestMainClass {

    public static void main(String[] args){
        try{
            final WebApi waFromScratch = webApiFromScratch();
            System.out.println("web aip from scratch to string: "+waFromScratch);
            final WebApi waFromStream = baseUnitFromStream();
            System.out.println("web aip from stream to string: "+waFromStream);
            final String waFromScratchDumped = dumpApiToStream(waFromScratch);
            System.out.println("Web api from scratch dumpped: "+waFromScratchDumped);

            final String waFromStreamDumped = dumpApiToStream(waFromStream);
            System.out.println("Web api from stream dumpped: "+waFromStreamDumped);

            dumpApiToFile(waFromStream);
            System.out.println("Web api from stream to file dumpped ");

        } catch (ExecutionException | InterruptedException e) {
            e.printStackTrace();
        }
    }


    private static WebApi webApiFromScratch(){
        System.out.println("write here your code for test the web api :)");
        List<String> list = Arrays.asList("one","two");
        WebApi webApi = new WebApi();

        webApi.withSchemes(list);
        List<EndPoint> endpoints = new ArrayList<>();
        webApi.withEndPoint("/endpoint")
                .withDescription("endpoint description")
                .withName("endpoint name")
                .withOperation("get")
                    .withDescription("operaiton description")
                    .withDocumentation( new CreativeWork()
                                            .withDescription("o1 description")
                                            .withUrl("o1 cw url"))
                    .withName("operation name");

        return webApi;
    }

    private static WebApi baseUnitFromStream() throws InterruptedException, ExecutionException {
        CompletableFuture<BaseUnit> future = new JvmClient().generateAsyncFromStream(completeRamlApi, HintHelper.ramlYamlHint());
        final BaseUnit baseUnit = future.get();
        return ((Document)baseUnit).encodes();
    }

    private static String dumpApiToStream(@NotNull WebApi webApi) throws ExecutionException, InterruptedException {
        Document build = new Document(webApi);
        CompletableFuture<String> stringFuture = new JvmGenerator().generateToStringAsync(build, VendorHelper.raml());
        return stringFuture.get();
    }

    private static void dumpApiToFile(@NotNull WebApi webApi) throws ExecutionException, InterruptedException {
        Document build = new Document(webApi);
        CompletableFuture<String> stringFuture = new JvmGenerator().
                generateToFileAsync(build, "file:///Users/hernan.najles/mulesoft/amf/usage/jvmClient/src/main/resources/output/output.json",VendorHelper.raml());
        stringFuture.get();
    }


    private static final String completeRamlApi = "#%RAML 1.0\n" +
            "title: test title\n" +
            "description: test description\n" +
            "(termsOfService): terms of service\n" +
            "version: 1.1\n" +
            "(license):\n" +
            "  url: licenseUrl\n" +
            "  name: licenseName\n" +
            "baseUri: http://api.example.com/path\n" +
            "mediaType:\n" +
            "  - application/yaml\n" +
            "protocols:\n" +
            "  - http\n" +
            "  - https\n" +
            "(contact):\n" +
            "  url: contactUrl\n" +
            "  name: contactName\n" +
            "  email: contactEmail\n" +
            "(externalDocs):\n" +
            "  url: externalDocsUrl\n" +
            "  description: externalDocsDescription";
}
